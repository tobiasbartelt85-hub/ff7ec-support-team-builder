module Core.Database
  ( init
  , createDbState
  , writeToCache

  -- Exported for tests
  , parseAndMigrateUserState
  , toSerializableUserState
  ) where

import Core.Database.Types
import Core.Database.UserState.VLatest
import Prelude

import Control.Alt (alt)
import Control.Apply (lift2)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Rec.Class (class MonadRec)
import Core.Database.UserState.V1 as V1
import Core.Database.UserState.VLatest as V2
import Core.Database.UserState.VLatest as VLatest
import Core.Display (display)
import Core.Weapons.Parser as P
import Core.Weapons.Parser as Parser
import Core.WebStorage as WS
import Data.Array as Arr
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NAR
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Either (Either(..), hush)
import Data.Foldable as F
import Data.List as List
import Data.List.Lazy as LazyList
import Data.List.ZipList (ZipList(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.String.NonEmpty as NES
import Data.Time.Duration (Hours(..))
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Now as Now
import Google.SheetsApi as SheetsApi
import Parsing (runParser)
import Partial.Unsafe (unsafeCrashWith)
import Utils (MapAsArray(..), SetAsArray(..), throwOnLeft, renderJsonErr, the, throwOnNothing, unsafeFromJust, whenJust)
import Yoga.JSON as J

currentUserStateVersion :: Int
currentUserStateVersion = 2

init :: Aff (Maybe DbState)
init = do
  runExceptT readFromCache >>= case _ of
    Left (err :: String) -> do
      Console.error err
      Console.log "Loading db from the spreadsheet..."
      dbMb <- hush <$> runExceptT (loadAndCreateDbState newUserState)
      whenJust dbMb $ writeToCache
      pure dbMb
    Right { userState, dbMaybe: Just { db, hasExpired } } | hasExpired -> do
      Console.log "Db found in cache but has expired, updating cache..."
      hush <$> runExceptT (loadAndCreateDbState userState) >>= case _ of
        Just updatedDbState -> do
          writeToCache updatedDbState
          pure $ Just updatedDbState
        Nothing -> do
          Console.error "Failed to update db, reusing existing expired db."
          pure $ Just { userState, db }
    Right { userState, dbMaybe: Nothing } -> do
      Console.log "Db not found in cache, updating cache..."
      hush <$> runExceptT (loadAndCreateDbState userState) >>= case _ of
        Just updatedDbState -> do
          writeToCache updatedDbState
          pure $ Just updatedDbState
        Nothing -> do
          Console.error "Failed to update db."
          pure $ Nothing
    Right { userState, dbMaybe: Just { db, hasExpired: _ } } -> do
      Console.log "Db found in cache."
      pure $ Just { userState, db }

  where
  -- Load the weapons from the spreadsheet, and updates the existing db.
  loadAndCreateDbState :: forall f. MonadAff f => MonadThrow Unit f => MonadRec f => UserState -> f DbState
  loadAndCreateDbState existingUserState = do
    weapons <- loadFromSpreadsheet
    createDbState weapons existingUserState

  -- Throws if we can't parse the Google Sheet.
  loadFromSpreadsheet :: forall f. MonadAff f => MonadThrow Unit f => f (Array Weapon)
  loadFromSpreadsheet = do
    table <- liftAff $ SheetsApi.getSheet "Weapons!A:ZZ"

    let { weapons, errors } = P.parseWeapons table.result.values
    for_ errors \err -> Console.log $ "Failed to parse weapon:\n" <> err

    when (Arr.null weapons) do
      Console.log "Failed to parse any weapons"
      throwError unit
    pure weapons

newUserState :: UserState
newUserState = { weapons: Map.empty }

getDistinctObs :: Weapon -> NonEmptyArray ObRange
getDistinctObs weapon = do
  NAR.cons' (weapon.ob0 /\ FromOb0 /\ ToOb0)
    [ weapon.ob1 /\ FromOb1 /\ ToOb5
    , weapon.ob6 /\ FromOb6 /\ ToOb9
    , weapon.ob10 /\ FromOb10 /\ ToOb10
    ]
    # NAR.groupBy (\(Tuple x _) (Tuple y _) -> areObLevelsEquivalent x y)
    <#> \(group :: NonEmptyArray (ObLevel /\ FromOb /\ ToOb)) ->
      ObRange
        { from: NAR.head group # \(Tuple _ (Tuple from _)) -> from
        , to: NAR.last group # \(Tuple _ (Tuple _ to)) -> to
        }
  where
  areObLevelsEquivalent :: ObLevel -> ObLevel -> Boolean
  areObLevelsEquivalent obx oby = do
    indices (Arr.length obx.effects)
      # Arr.all \idx -> do
          let effect1 = Arr.index obx.effects idx `unsafeFromJust` ("Index out of bounds for weapon: " <> display weapon.name)
          let effect2 = Arr.index oby.effects idx `unsafeFromJust` ("Index out of bounds for weapon: " <> display weapon.name)
          areWeaponEffectsEquivalent effect1 effect2

  indices :: Int -> Array Int
  indices n = if n <= 0 then [] else Arr.range 0 (n - 1)

  -- Two effects are equivalent if they're the same kind of effect with the same
  -- range and potencies. Duration, extension, and percentages are not considered.
  areWeaponEffectsEquivalent :: WeaponEffect -> WeaponEffect -> Boolean
  areWeaponEffectsEquivalent x y
    | tagOf x /= tagOf y =
        -- INVARIANT: We assume a weapon's effects are always listed in the same order at every overboost level.
        -- This function crashes if that invariant is violated.
        -- #(ref:effects-same-order)
        unsafeCrashWith $ "Effects for weapon " <> display weapon.name <> " are not in the same order"
    | otherwise = rangeOf x == rangeOf y && potenciesOf x == potenciesOf y

createDbState :: forall m. MonadEffect m => MonadRec m => Array Weapon -> UserState -> m DbState
createDbState newWeapons existingUserState = do
  (finalDb :: Db) <- Arr.foldRecM
    (\db weapon -> insertWeapon weapon db)
    newDb
    newWeapons

  let
    (finalUserState :: UserState) =
      F.foldl
        ( \userState weaponData ->
            case Map.lookup weaponData.weapon.name userState.weapons of
              -- If new weapons were added to the db, we need to create "empty states" for them.
              Nothing -> do
                let
                  updatedUserStateWeapons =
                    Map.insert weaponData.weapon.name
                      { ignored: false
                      , ownedOb: Just $ NAR.last weaponData.distinctObs
                      }
                      userState.weapons
                userState { weapons = updatedUserStateWeapons }
              Just existingWeaponState -> do
                -- @(ref:owned-ob-invariant)
                --
                -- Enforce the `UserStateWeapon.ownedOb` invariant: it must match one of
                -- the items in the corresponding `WeaponData.distinctObs`.
                -- If it doesn't, reset it.
                --
                -- This can happen when a new weapon with new effects is added to the sheet
                -- (and we set `distinctObs` to [OB0-10] and `ownedOb` to OB0-10),
                -- and then later we add support for that new effect,
                -- which changes the `distinctObs` for that weapon to e.g. [OB0-5, OB6-10].
                -- In that scenario, we have to manually correct the `ownedOb` to OB6-10.
                let
                  ownedObIsValid =
                    case existingWeaponState.ownedOb of
                      Just ownedOb -> NAR.elem ownedOb weaponData.distinctObs
                      Nothing -> true
                if ownedObIsValid then userState
                else do
                  let
                    updatedUserStateWeapons =
                      Map.insert weaponData.weapon.name
                        (existingWeaponState { ownedOb = Just $ NAR.last weaponData.distinctObs })
                        userState.weapons
                  userState { weapons = updatedUserStateWeapons }
        )
        existingUserState
        finalDb.allWeapons

  pure { db: finalDb, userState: finalUserState }

  where
  newDb :: Db
  newDb =
    { allWeapons: Map.empty
    , groupedByEffect: Map.empty
    , allCharacterNames: Set.empty
    }

insertWeapon
  :: forall m
   . MonadEffect m
  => Weapon
  -> Db
  -> m Db
insertWeapon weapon db = do
  let groups = groupsForWeapon weapon
  if List.null groups then pure db
  else
    pure $ db
      # insert
      # insertIntoGroups groups
      # insertCharacterName
  where

  insert :: Db -> Db
  insert db = do
    let distinctObs = getDistinctObs weapon
    let
      newWeapon =
        { weapon
        , distinctObs
        }
    db { allWeapons = Map.insert weapon.name newWeapon db.allWeapons }

  insertIntoGroups :: List.List GroupEntry -> Db -> Db
  insertIntoGroups groups db =
    F.foldr
      insertIntoGroup
      db
      groups

  insertIntoGroup :: GroupEntry -> Db -> Db
  insertIntoGroup { effectType, groupedWeapon } db = do
    let
      groupedByEffect = Map.alter
        ( case _ of
            Just weapons -> Just $ Arr.snoc weapons groupedWeapon
            Nothing -> Just [ groupedWeapon ]
        )
        effectType
        db.groupedByEffect
    db { groupedByEffect = groupedByEffect }

  insertCharacterName :: Db -> Db
  insertCharacterName db =
    db { allCharacterNames = Set.insert weapon.character db.allCharacterNames }

type GroupEntry =
  { effectType :: FilterEffectType
  , groupedWeapon :: GroupedWeapon
  }

-- The range of an effect, if it has one. Most `WeaponEffect` constructors carry a
-- `range` field, but a few (e.g. `IncreaseCommandGauge`) have no range in the game
-- data, in which case this returns `Nothing` (and they're grouped with `ranges: Nothing`).
rangeOf :: WeaponEffect -> Maybe Range
rangeOf = case _ of
  Heal r -> Just r.range
  PatkUp r -> Just r.range
  MatkUp r -> Just r.range
  PdefUp r -> Just r.range
  MdefUp r -> Just r.range
  IncreaseCommandGauge _ -> Nothing
  HPGain r -> Just r.range
  EnhanceBuffs r -> Just r.range
  PhysicalWeaponBoost r -> Just r.range
  MagicWeaponBoost r -> Just r.range
  PhysicalDamageBonus r -> Just r.range
  MagicDamageBonus r -> Just r.range
  PhysATBConservationEffect r -> Just r.range
  MagATBConservationEffect r -> Just r.range
  AmpPhysAbilities r -> Just r.range
  AmpMagAbilities r -> Just r.range
  FireDamageUp r -> Just r.range
  IceDamageUp r -> Just r.range
  LightningDamageUp r -> Just r.range
  EarthDamageUp r -> Just r.range
  WaterDamageUp r -> Just r.range
  WindDamageUp r -> Just r.range
  FireWeaponBoost r -> Just r.range
  IceWeaponBoost r -> Just r.range
  LightningWeaponBoost r -> Just r.range
  EarthWeaponBoost r -> Just r.range
  WaterWeaponBoost r -> Just r.range
  WindWeaponBoost r -> Just r.range
  FireDamageBonus r -> Just r.range
  IceDamageBonus r -> Just r.range
  LightningDamageBonus r -> Just r.range
  EarthDamageBonus r -> Just r.range
  WaterDamageBonus r -> Just r.range
  WindDamageBonus r -> Just r.range
  FireATBConservationEffect r -> Just r.range
  IceATBConservationEffect r -> Just r.range
  LightningATBConservationEffect r -> Just r.range
  EarthATBConservationEffect r -> Just r.range
  WaterATBConservationEffect r -> Just r.range
  WindATBConservationEffect r -> Just r.range
  AmpFireAbilities r -> Just r.range
  AmpIceAbilities r -> Just r.range
  AmpLightningAbilities r -> Just r.range
  AmpEarthAbilities r -> Just r.range
  AmpWaterAbilities r -> Just r.range
  AmpWindAbilities r -> Just r.range
  FireResistUp r -> Just r.range
  IceResistUp r -> Just r.range
  LightningResistUp r -> Just r.range
  EarthResistUp r -> Just r.range
  WaterResistUp r -> Just r.range
  WindResistUp r -> Just r.range
  Veil r -> Just r.range
  Provoke r -> Just r.range
  PatkDown r -> Just r.range
  MatkDown r -> Just r.range
  PdefDown r -> Just r.range
  MdefDown r -> Just r.range
  SingleTgtPhysDmgRcvdUp r -> Just r.range
  SingleTgtMagDmgRcvdUp r -> Just r.range
  AllTgtPhysDmgRcvdUp r -> Just r.range
  AllTgtMagDmgRcvdUp r -> Just r.range
  FireDamageDown r -> Just r.range
  IceDamageDown r -> Just r.range
  LightningDamageDown r -> Just r.range
  EarthDamageDown r -> Just r.range
  WaterDamageDown r -> Just r.range
  WindDamageDown r -> Just r.range
  FireResistDown r -> Just r.range
  IceResistDown r -> Just r.range
  LightningResistDown r -> Just r.range
  EarthResistDown r -> Just r.range
  WaterResistDown r -> Just r.range
  WindResistDown r -> Just r.range
  SingleTgtFireDmgRcvdUp r -> Just r.range
  SingleTgtIceDmgRcvdUp r -> Just r.range
  SingleTgtLightningDmgRcvdUp r -> Just r.range
  SingleTgtEarthDmgRcvdUp r -> Just r.range
  SingleTgtWaterDmgRcvdUp r -> Just r.range
  SingleTgtWindDmgRcvdUp r -> Just r.range
  AllTgtFireDmgRcvdUp r -> Just r.range
  AllTgtIceDmgRcvdUp r -> Just r.range
  AllTgtLightningDmgRcvdUp r -> Just r.range
  AllTgtEarthDmgRcvdUp r -> Just r.range
  AllTgtWaterDmgRcvdUp r -> Just r.range
  AllTgtWindDmgRcvdUp r -> Just r.range
  FireWeakness r -> Just r.range
  IceWeakness r -> Just r.range
  LightningWeakness r -> Just r.range
  EarthWeakness r -> Just r.range
  WaterWeakness r -> Just r.range
  WindWeakness r -> Just r.range
  Enfeeble r -> Just r.range
  Stop r -> Just r.range
  ExploitWeakness r -> Just r.range
  EnhanceDebuffs r -> Just r.range
  Enliven r -> Just r.range

-- The potencies of an effect, if it has any.
-- Some effects have no potencies (e.g. `Heal`, `Provoke`, `Enliven`,
-- and the percentage-based boosts), in which case this returns `Nothing`.
potenciesOf :: WeaponEffect -> Maybe Potencies
potenciesOf = case _ of
  Heal _ -> Nothing
  PatkUp r -> Just r.potencies
  MatkUp r -> Just r.potencies
  PdefUp r -> Just r.potencies
  MdefUp r -> Just r.potencies
  IncreaseCommandGauge _ -> Nothing
  HPGain _ -> Nothing
  EnhanceBuffs r -> Just r.potencies
  PhysicalWeaponBoost _ -> Nothing
  MagicWeaponBoost _ -> Nothing
  PhysicalDamageBonus _ -> Nothing
  MagicDamageBonus _ -> Nothing
  PhysATBConservationEffect _ -> Nothing
  MagATBConservationEffect _ -> Nothing
  AmpPhysAbilities _ -> Nothing
  AmpMagAbilities _ -> Nothing
  FireDamageUp r -> Just r.potencies
  IceDamageUp r -> Just r.potencies
  LightningDamageUp r -> Just r.potencies
  EarthDamageUp r -> Just r.potencies
  WaterDamageUp r -> Just r.potencies
  WindDamageUp r -> Just r.potencies
  FireWeaponBoost _ -> Nothing
  IceWeaponBoost _ -> Nothing
  LightningWeaponBoost _ -> Nothing
  EarthWeaponBoost _ -> Nothing
  WaterWeaponBoost _ -> Nothing
  WindWeaponBoost _ -> Nothing
  FireDamageBonus _ -> Nothing
  IceDamageBonus _ -> Nothing
  LightningDamageBonus _ -> Nothing
  EarthDamageBonus _ -> Nothing
  WaterDamageBonus _ -> Nothing
  WindDamageBonus _ -> Nothing
  FireATBConservationEffect _ -> Nothing
  IceATBConservationEffect _ -> Nothing
  LightningATBConservationEffect _ -> Nothing
  EarthATBConservationEffect _ -> Nothing
  WaterATBConservationEffect _ -> Nothing
  WindATBConservationEffect _ -> Nothing
  AmpFireAbilities _ -> Nothing
  AmpIceAbilities _ -> Nothing
  AmpLightningAbilities _ -> Nothing
  AmpEarthAbilities _ -> Nothing
  AmpWaterAbilities _ -> Nothing
  AmpWindAbilities _ -> Nothing
  FireResistUp r -> Just r.potencies
  IceResistUp r -> Just r.potencies
  LightningResistUp r -> Just r.potencies
  EarthResistUp r -> Just r.potencies
  WaterResistUp r -> Just r.potencies
  WindResistUp r -> Just r.potencies
  Veil _ -> Nothing
  Provoke _ -> Nothing
  PatkDown r -> Just r.potencies
  MatkDown r -> Just r.potencies
  PdefDown r -> Just r.potencies
  MdefDown r -> Just r.potencies
  SingleTgtPhysDmgRcvdUp _ -> Nothing
  SingleTgtMagDmgRcvdUp _ -> Nothing
  AllTgtPhysDmgRcvdUp _ -> Nothing
  AllTgtMagDmgRcvdUp _ -> Nothing
  FireDamageDown r -> Just r.potencies
  IceDamageDown r -> Just r.potencies
  LightningDamageDown r -> Just r.potencies
  EarthDamageDown r -> Just r.potencies
  WaterDamageDown r -> Just r.potencies
  WindDamageDown r -> Just r.potencies
  FireResistDown r -> Just r.potencies
  IceResistDown r -> Just r.potencies
  LightningResistDown r -> Just r.potencies
  EarthResistDown r -> Just r.potencies
  WaterResistDown r -> Just r.potencies
  WindResistDown r -> Just r.potencies
  SingleTgtFireDmgRcvdUp _ -> Nothing
  SingleTgtIceDmgRcvdUp _ -> Nothing
  SingleTgtLightningDmgRcvdUp _ -> Nothing
  SingleTgtEarthDmgRcvdUp _ -> Nothing
  SingleTgtWaterDmgRcvdUp _ -> Nothing
  SingleTgtWindDmgRcvdUp _ -> Nothing
  AllTgtFireDmgRcvdUp _ -> Nothing
  AllTgtIceDmgRcvdUp _ -> Nothing
  AllTgtLightningDmgRcvdUp _ -> Nothing
  AllTgtEarthDmgRcvdUp _ -> Nothing
  AllTgtWaterDmgRcvdUp _ -> Nothing
  AllTgtWindDmgRcvdUp _ -> Nothing
  FireWeakness _ -> Nothing
  IceWeakness _ -> Nothing
  LightningWeakness _ -> Nothing
  EarthWeakness _ -> Nothing
  WaterWeakness _ -> Nothing
  WindWeakness _ -> Nothing
  Enfeeble _ -> Nothing
  Stop _ -> Nothing
  ExploitWeakness _ -> Nothing
  EnhanceDebuffs r -> Just r.potencies
  Enliven _ -> Nothing

-- A tag identifying which kind of effect this is (i.e. its constructor), so two
-- effects can be checked for "same kind" regardless of their range or potencies.
tagOf :: WeaponEffect -> FilterEffectType
tagOf = case _ of
  Heal _ -> FilterHeal
  PatkUp _ -> FilterPatkUp
  MatkUp _ -> FilterMatkUp
  PdefUp _ -> FilterPdefUp
  MdefUp _ -> FilterMdefUp
  IncreaseCommandGauge _ -> FilterIncreaseCommandGauge
  HPGain _ -> FilterHPGain
  EnhanceBuffs _ -> FilterEnhanceBuffs
  PhysicalWeaponBoost _ -> FilterPhysicalWeaponBoost
  MagicWeaponBoost _ -> FilterMagicWeaponBoost
  PhysicalDamageBonus _ -> FilterPhysicalDamageBonus
  MagicDamageBonus _ -> FilterMagicDamageBonus
  PhysATBConservationEffect _ -> FilterPhysATBConservationEffect
  MagATBConservationEffect _ -> FilterMagATBConservationEffect
  AmpPhysAbilities  _ -> FilterAmpPhysAbilities
  AmpMagAbilities _ -> FilterAmpMagAbilities
  FireDamageUp _ -> FilterFireDamageUp
  IceDamageUp _ -> FilterIceDamageUp
  LightningDamageUp _ -> FilterLightningDamageUp
  EarthDamageUp _ -> FilterEarthDamageUp
  WaterDamageUp _ -> FilterWaterDamageUp
  WindDamageUp _ -> FilterWindDamageUp
  FireWeaponBoost _ -> FilterFireWeaponBoost
  IceWeaponBoost _ -> FilterIceWeaponBoost
  LightningWeaponBoost _ -> FilterLightningWeaponBoost
  EarthWeaponBoost _ -> FilterEarthWeaponBoost
  WaterWeaponBoost _ -> FilterWaterWeaponBoost
  WindWeaponBoost _ -> FilterWindWeaponBoost
  FireDamageBonus _ -> FilterFireDamageBonus
  IceDamageBonus _ -> FilterIceDamageBonus
  LightningDamageBonus _ -> FilterLightningDamageBonus
  EarthDamageBonus _ -> FilterEarthDamageBonus
  WaterDamageBonus _ -> FilterWaterDamageBonus
  WindDamageBonus _ -> FilterWindDamageBonus
  FireATBConservationEffect _ -> FilterFireATBConservationEffect
  IceATBConservationEffect _ -> FilterIceATBConservationEffect
  LightningATBConservationEffect _ -> FilterLightningATBConservationEffect
  EarthATBConservationEffect _ -> FilterEarthATBConservationEffect
  WaterATBConservationEffect _ -> FilterWaterATBConservationEffect
  WindATBConservationEffect _ -> FilterWindATBConservationEffect
  AmpFireAbilities  _ -> FilterAmpFireAbilities
  AmpIceAbilities _ -> FilterAmpIceAbilities
  AmpLightningAbilities _ -> FilterAmpLightningAbilities
  AmpEarthAbilities _ -> FilterAmpEarthAbilities
  AmpWaterAbilities _ -> FilterAmpWaterAbilities
  AmpWindAbilities _ -> FilterAmpWindAbilities
  FireResistUp _ -> FilterFireResistUp
  IceResistUp _ -> FilterIceResistUp
  LightningResistUp _ -> FilterLightningResistUp
  EarthResistUp _ -> FilterEarthResistUp
  WaterResistUp _ -> FilterWaterResistUp
  WindResistUp _ -> FilterWindResistUp
  Veil _ -> FilterVeil
  Provoke _ -> FilterProvoke
  PatkDown _ -> FilterPatkDown
  MatkDown _ -> FilterMatkDown
  PdefDown _ -> FilterPdefDown
  MdefDown _ -> FilterMdefDown
  SingleTgtPhysDmgRcvdUp _ -> FilterSingleTgtPhysDmgRcvdUp
  SingleTgtMagDmgRcvdUp _ -> FilterSingleTgtMagDmgRcvdUp
  AllTgtPhysDmgRcvdUp _ -> FilterAllTgtPhysDmgRcvdUp
  AllTgtMagDmgRcvdUp _ -> FilterAllTgtMagDmgRcvdUp
  FireDamageDown _ -> FilterFireDamageDown
  IceDamageDown _ -> FilterIceDamageDown
  LightningDamageDown _ -> FilterLightningDamageDown
  EarthDamageDown _ -> FilterEarthDamageDown
  WaterDamageDown _ -> FilterWaterDamageDown
  WindDamageDown _ -> FilterWindDamageDown
  FireResistDown _ -> FilterFireResistDown
  IceResistDown _ -> FilterIceResistDown
  LightningResistDown _ -> FilterLightningResistDown
  EarthResistDown _ -> FilterEarthResistDown
  WaterResistDown _ -> FilterWaterResistDown
  WindResistDown _ -> FilterWindResistDown
  SingleTgtFireDmgRcvdUp _ -> FilterSingleTgtFireDmgRcvdUp
  SingleTgtIceDmgRcvdUp _ -> FilterSingleTgtIceDmgRcvdUp
  SingleTgtLightningDmgRcvdUp _ -> FilterSingleTgtLightningDmgRcvdUp
  SingleTgtEarthDmgRcvdUp _ -> FilterSingleTgtEarthDmgRcvdUp
  SingleTgtWaterDmgRcvdUp _ -> FilterSingleTgtWaterDmgRcvdUp
  SingleTgtWindDmgRcvdUp _ -> FilterSingleTgtWindDmgRcvdUp
  AllTgtFireDmgRcvdUp _ -> FilterAllTgtFireDmgRcvdUp
  AllTgtIceDmgRcvdUp _ -> FilterAllTgtIceDmgRcvdUp
  AllTgtLightningDmgRcvdUp _ -> FilterAllTgtLightningDmgRcvdUp
  AllTgtEarthDmgRcvdUp _ -> FilterAllTgtEarthDmgRcvdUp
  AllTgtWaterDmgRcvdUp _ -> FilterAllTgtWaterDmgRcvdUp
  AllTgtWindDmgRcvdUp _ -> FilterAllTgtWindDmgRcvdUp
  FireWeakness _ -> FilterFireWeakness
  IceWeakness _ -> FilterIceWeakness
  LightningWeakness _ -> FilterLightningWeakness
  EarthWeakness _ -> FilterEarthWeakness
  WaterWeakness _ -> FilterWaterWeakness
  WindWeakness _ -> FilterWindWeakness
  Enfeeble _ -> FilterEnfeeble
  Stop _ -> FilterStop
  ExploitWeakness _ -> FilterExploitWeakness
  EnhanceDebuffs _ -> FilterEnhanceDebuffs
  Enliven _ -> FilterEnliven

groupsForWeapon :: Weapon -> List.List GroupEntry
groupsForWeapon weapon = do
  mergeRanges $ Arr.fold
    [ LazyList.catMaybes (unwrap groupsForWeapon')
    , getCureAllAbility
    , getCommandAbilityDiamondSigil
    , getSigilBoost
    ]
  where
  -- Check if the weapon has a Cure All S-Ability.
  -- #(ref:use-cure-spell)
  getCureAllAbility :: LazyList.List GroupEntry
  getCureAllAbility = do
    let sAbilities = [ weapon.sAbilities.slot1, weapon.sAbilities.slot2, weapon.sAbilities.slot3 ]
    if Arr.any Parser.hasCureAllSAbility sAbilities then
      LazyList.singleton
        { effectType: FilterHeal
        , groupedWeapon:
            { weaponName: weapon.name
            , ranges: Just
                [ { allRanges: { ob0: All, ob1: All, ob6: All, ob10: All }
                  , allPotencies: Nothing
                  }
                ]
            }
        }
    else
      LazyList.nil

  -- Check if the weapon has a C. Ability Diamond Sigil.
  getCommandAbilityDiamondSigil :: LazyList.List GroupEntry
  getCommandAbilityDiamondSigil =
    case weapon.commandAbilitySigil of
      Just SigilDiamond -> LazyList.singleton
        { effectType: FilterSigilDiamond
        , groupedWeapon:
            { weaponName: weapon.name
            , ranges: Nothing
            }
        }
      _ -> LazyList.nil

  -- Check if the weapon has a Sigil Boost S. Ability.
  -- #(ref:use-sigil-boosts)
  getSigilBoost :: LazyList.List GroupEntry
  getSigilBoost =
    LazyList.fromFoldable [ weapon.sAbilities.slot1, weapon.sAbilities.slot2, weapon.sAbilities.slot3 ]
      <#> (\sAbility -> hush $ runParser (NES.toString sAbility) Parser.parseSAbilitySigilBoost)
      # LazyList.catMaybes
      <#> \sigil ->
        { effectType:
            case sigil of
              SigilO -> FilterSigilBoostO
              SigilX -> FilterSigilBoostX
              SigilTriangle -> FilterSigilBoostTriangle
              SigilDiamond -> FilterSigilDiamond
        , groupedWeapon:
            { weaponName: weapon.name
            , ranges: Nothing
            }
        }

  groupsForWeapon' :: ZipList (Maybe GroupEntry)
  groupsForWeapon' = ado
    -- INVARIANT: this assumes weapon effects are listed in the same order at all overboost levels.
    -- @(ref:effects-same-order)
    ob0 <- ZipList $ LazyList.fromFoldable weapon.ob0.effects
    ob1 <- ZipList $ LazyList.fromFoldable weapon.ob1.effects
    ob6 <- ZipList $ LazyList.fromFoldable weapon.ob6.effects
    ob10 <- ZipList $ LazyList.fromFoldable weapon.ob10.effects
    in
      groupForWeaponEffect ob0 ob1 ob6 ob10 <#> \{ effectType, potencies, allRanges } ->
        { effectType
        , groupedWeapon:
            { weaponName: weapon.name
            -- Effects with no range (e.g. `IncreaseCommandGauge`) get `ranges: Nothing`,
            -- like Sigil effects. Dropping `potencies` along with the range here is safe
            -- because of @(ref:potencies-have-range).
            , ranges: allRanges <#> \ranges ->
                [ { allRanges: ranges
                  , allPotencies: potencies
                  }
                ]
            }
        }

  -- If there are many ranges for the same effect (e.g. Arctic Star has PATK Up SingleTarget & PATK Up Self),
  -- this function will merge those `GroupEntry`s (with one `GroupedWeaponRange` each) into a single one (with many `GroupedWeaponRange`s).
  mergeRanges :: LazyList.List GroupEntry -> List.List GroupEntry
  mergeRanges groupEntries = do
    let
      (merged :: Map FilterEffectType GroupEntry) =
        LazyList.foldl
          ( \map ge ->
              Map.alter
                ( case _ of
                    Nothing -> Just ge
                    Just existingGe ->
                      Just $ existingGe
                        { groupedWeapon
                            { ranges =
                                existingGe.groupedWeapon.ranges
                                  <>
                                    ge.groupedWeapon.ranges
                            }
                        }
                )
                ge.effectType
                map
          )
          Map.empty
          groupEntries
    Map.values merged

  groupForWeaponEffect
    :: WeaponEffect
    -> WeaponEffect
    -> WeaponEffect
    -> WeaponEffect
    -> Maybe
         { effectType :: FilterEffectType
         , potencies :: Maybe AllPotencies
         , allRanges :: Maybe AllRanges
         }
  -- NOTE: an effect can have a different range and different potencies at each
  -- overboost level (e.g. Festive Sword's Enliven is `Self` at OB0/OB1 and `All`
  -- at OB6/OB10), so we read both from every level via `rangeOf` / `potenciesOf`.
  -- Only the `effectType` is determined from the OB0 effect.
  --
  -- `allRanges` is `Nothing` for effects that have no range (e.g. `IncreaseCommandGauge`).
  --
  -- This assumes effects are listed in the same order at all overboost levels.
  -- That invariant is enforced here: @(ref:effects-same-order)
  groupForWeaponEffect ob0 ob1 ob6 ob10 =
    effectTypeOf <#> \effectType ->
      { effectType
      , potencies: allPotencies
      , allRanges
      }
    where
    -- `Just` only for effects that have potencies; `Nothing` otherwise.
    allPotencies = do
      ob0Potencies <- potenciesOf ob0
      ob1Potencies <- potenciesOf ob1
      ob6Potencies <- potenciesOf ob6
      ob10Potencies <- potenciesOf ob10
      pure { ob0: ob0Potencies, ob1: ob1Potencies, ob6: ob6Potencies, ob10: ob10Potencies }

    -- `Just` only for effects that have a range; `Nothing` otherwise.
    allRanges = do
      ob0Range <- rangeOf ob0
      ob1Range <- rangeOf ob1
      ob6Range <- rangeOf ob6
      ob10Range <- rangeOf ob10
      pure { ob0: ob0Range, ob1: ob1Range, ob6: ob6Range, ob10: ob10Range }

    effectTypeOf = case ob0 of
      -- #(ref:heal-threshold)
      Heal { percentage } -> if unwrap percentage >= 30 then Just FilterHeal else Nothing
      _ -> Just (tagOf ob0)

type ReadCacheResult =
  { userState :: UserState
  , dbMaybe ::
      Maybe
        { db :: Db
        , hasExpired :: Boolean
        }
  }

-- Throws if the cache is empty OR the cache data is corrupted.
readFromCache :: forall m. MonadThrow String m => MonadAff m => m ReadCacheResult
readFromCache = do
  -- NOTE: in V1, we used to store the version number in `db_version`.
  -- From V2 onwards, it's stored in `user_state_version`.
  userStateVersionStr <- lift2 alt (WS.getItem "user_state_version") (WS.getItem "db_version")
    >>= throwOnNothing \_ -> "'user_state_version' / `db_version` not found in cache"
  userStateVersion :: Int <- J.readJSON userStateVersionStr
    # throwOnLeft \err -> "Failed to deserialize 'user_state_version':\n" <> renderJsonErr err

  lastUpdatedStr <- WS.getItem "last_updated"
    >>= throwOnNothing \_ -> "'last_updated' not found in cache"
  lastUpdated :: DateTime <- J.readJSON lastUpdatedStr
    # throwOnLeft \err -> "Failed to deserialize 'last_updated':\n" <> renderJsonErr err

  dbStr <- WS.getItem "db"
    >>= throwOnNothing \_ -> "'db' not found in cache"
  userStateStr <- WS.getItem "user_state" >>= case _ of
    Just userStateStr -> pure userStateStr
    Nothing | userStateVersion == 1 ->
      -- NOTE: in V1, the user state used to be stored in the `db` cache key, so we deserialize it from `dbStr`
      -- From V2 onwards, it's stored in the `user_state` key
      pure dbStr
    Nothing -> do
      throwError "`user_state` not found in cache, even though `db` was found."

  userState :: VLatest.UserState <- parseAndMigrateUserState userStateStr userStateVersion

  dbMaybe <- case fromSerializableDb <$> J.readJSON dbStr of
    Right (db :: Db) -> do
      now <- liftEffect Now.nowDateTime
      pure $ Just
        { db
        , hasExpired: DateTime.diff now lastUpdated > Hours 24.0
        }
    Left err -> do
      Console.error "Failed to deserializable db. The schema may have recently changed."
      Console.error $ renderJsonErr err
      pure Nothing

  pure { userState, dbMaybe }

  where
  fromSerializableDb :: SerializableDb -> Db
  fromSerializableDb db =
    { allWeapons: unwrap db.allWeapons
    , groupedByEffect: unwrap db.groupedByEffect
    , allCharacterNames: unwrap db.allCharacterNames
    }

parseAndMigrateUserState :: forall m. MonadThrow String m => String -> Int -> m VLatest.UserState
parseAndMigrateUserState userStateStr userStateVersion = do
  case userStateVersion of
    1 -> do
      J.readJSON userStateStr
        # throwOnLeft (\err -> "Failed to deserialize db:\n" <> renderJsonErr err)
        <#> V1.deserializeUserState
        <#> the @V1.UserState
        <#> V2.migrate
    2 -> do
      J.readJSON userStateStr
        # throwOnLeft (\err -> "Failed to deserialize user_state:\n" <> renderJsonErr err)
        <#> V2.deserializeUserState
        <#> the @V2.UserState
    _ -> do
      throwError $ "Unexpected user state version number: " <> show userStateVersion

writeToCache :: forall m. MonadAff m => DbState -> m Unit
writeToCache dbState = do
  let dbStr = J.writeJSON $ toSerializableDb dbState.db
  let userStateStr = J.writeJSON $ toSerializableUserState dbState.userState
  lastUpdatedStr <- J.writeJSON <$> liftEffect Now.nowDateTime
  let currentUserStateVersionStr = J.writeJSON currentUserStateVersion

  WS.setItem "db" dbStr
  WS.setItem "user_state" userStateStr
  WS.setItem "last_updated" lastUpdatedStr
  WS.setItem "user_state_version" currentUserStateVersionStr
  where
  toSerializableDb :: Db -> SerializableDb
  toSerializableDb db =
    { allWeapons: MapAsArray db.allWeapons
    , groupedByEffect: MapAsArray db.groupedByEffect
    , allCharacterNames: SetAsArray db.allCharacterNames
    }

toSerializableUserState :: UserState -> SerializableUserState
toSerializableUserState userState =
  { weapons: MapAsArray userState.weapons
  }
