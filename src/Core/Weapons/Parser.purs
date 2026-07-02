module Core.Weapons.Parser where

import Core.Database.Types
import Core.Database.UserState.VLatest
import Prelude

import Control.Alt ((<|>))
import Data.Array as Arr
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NAR
import Data.Bifunctor (lmap)
import Data.Either (Either(..), hush)
import Data.Foldable (foldMap)
import Data.FoldableWithIndex as F
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.String.Utils as String
import Data.Tuple (Tuple(..))
import Parsing (runParser)
import Parsing as P
import Parsing.Combinators as P
import Parsing.String as P
import Parsing.String.Basic as P
import Utils (unsafeFromJust)

type ParseResult =
  { weapons ::
      Array Weapon
  , errors :: Array String
  }

type Result a = Either String a

type Parser a = P.Parser String a

parseWeapons :: Array (Array String) -> ParseResult
parseWeapons lines =
  lines
    # Arr.drop 1 -- Skip the 1st row with the column headers
    # F.foldlWithIndex
        ( \rowIndex result row ->
            case parseWeapon rowIndex row of
              Right weapons -> result { weapons = result.weapons <> NAR.toArray weapons }
              Left err -> result { errors = Arr.snoc result.errors err }
        )
        { weapons: [], errors: [] }

-- | Parses a single spreadsheet row into one or more `Weapon`s.
--
-- Most rows produce exactly one weapon. However, a weapon with "Club Custom"
-- columns (the 4 rightmost columns) produces *two* weapons: the original weapon,
-- and a variant whose C. Ability (columns S through V) is replaced by the Club
-- Custom columns. The variant's name is suffixed with " (Club Custom)".
-- #(ref:club-custom)
parseWeapon :: Int -> Array String -> Result (NonEmptyArray Weapon)
parseWeapon rowIndex row = do
  name <- WeaponName <$> getCell 0
  character <- CharacterName <$> getCell 1
  source <- getCell 2
  { obLevel: ob0 } <- getDescription 18
  { obLevel: ob1 } <- getDescription 19
  { obLevel: ob6 } <- getDescription 20
  { obLevel: ob10, atbCost, commandAbilitySigil } <- getDescription 21
  image <- getCell 22

  rAbility1 <- getCell 3
  rAbility2 <- getCell 5

  sAbility1 <- getCell 23
  sAbility2 <- getCell 24
  sAbility3 <- getCell 25

  let diamondCustomDescription = getOptionalCell 34

  {-
    NOTE: we only parse the text for R.Abilities so we can display them in the UI.
    NOTE: we only parse the text for Diamond Custom Abilities so we can display them in the UI.
    NOTE: for S.Abilities, we only care about:
      * sigil boosts.
        * `parseSAbilitySigilBoost` is used in `Database.groupsForWeapon`
        * @(ref:parse-sigil-boosts)
        * @(ref:use-sigil-boosts)
      * All (Cure Spells)
        * `hasCureAllSAbility``
        * @(ref:parse-cure-spell)
        * @(ref:use-cure-spell)

  -}

  let
    mkWeapon weaponName obLevels =
      { name: weaponName
      , character
      , source
      , image
      , atbCost: obLevels.atbCost
      , ob0: obLevels.ob0
      , ob1: obLevels.ob1
      , ob6: obLevels.ob6
      , ob10: obLevels.ob10
      , commandAbilitySigil: obLevels.commandAbilitySigil
      , sAbilities: { slot1: sAbility1, slot2: sAbility2, slot3: sAbility3 }
      , rAbilities: { slot1: rAbility1, slot2: rAbility2 }
      , diamondCustomDescription
      }

    weapon = mkWeapon name { atbCost, ob0, ob1, ob6, ob10, commandAbilitySigil }

  -- A weapon with Club Custom columns produces an extra "(Club Custom)" variant.
  -- @(ref:club-custom)
  clubWeapon <-
    case getOptionalCell 35 of
      Nothing -> pure Nothing
      Just _ -> do
        { obLevel: clubOb0 } <- getClubDescription 35
        { obLevel: clubOb1 } <- getClubDescription 36
        { obLevel: clubOb6 } <- getClubDescription 37
        { obLevel: clubOb10, atbCost: clubAtbCost, commandAbilitySigil: clubSigil } <- getClubDescription 38
        let clubName = WeaponName $ NES.appendString (unwrap name) " (Club Custom)"
        pure $ Just $ mkWeapon clubName
          { atbCost: clubAtbCost
          , ob0: clubOb0
          , ob1: clubOb1
          , ob6: clubOb6
          , ob10: clubOb10
          , commandAbilitySigil: clubSigil
          }

  pure $ NAR.cons' weapon $ Arr.fromFoldable clubWeapon
  where
  rowId = rowIndex + 1

  getDescription :: Int -> Result ParsedDescription
  getDescription columnIndex = do
    let
      columnId = columnIndex + 1
      heartCustomDescription = getOptionalCell (columnIndex + 8)
      spadeCustomDescription = getOptionalCell (columnIndex + 12)
    getCell columnIndex >>= parseDescription { rowId, columnId } heartCustomDescription spadeCustomDescription

  -- Like `getDescription`, but for the Club Custom columns. The Club Custom
  -- *replaces* the original C. Ability entirely, so the Heart/Spade customizations
  -- (which add to the original ability) don't apply to this variant.
  -- @(ref:club-custom)
  getClubDescription :: Int -> Result ParsedDescription
  getClubDescription columnIndex = do
    let columnId = columnIndex + 1
    getCell columnIndex >>= parseDescription { rowId, columnId }
      -- #(ref:club-heart-spade-mutually-exclusive)
      Nothing
      Nothing

  getCell :: Int -> Result NonEmptyString
  getCell columnIndex = do
    let
      columnId = columnIndex + 1
    str <-
      Arr.index row columnIndex
        `onErr`
          ("Row " <> show rowId <> " does not contain a cell at column " <> show columnId)
    NES.fromString str
      `onErr`
        ("Cell at " <> show rowId <> ":" <> show columnId <> " is empty")

  getOptionalCell :: Int -> Maybe NonEmptyString
  getOptionalCell columnIndex = Arr.index row columnIndex >>= NES.fromString

infixl 0 onErr as !@

onErr :: forall a. Maybe a -> String -> Either String a
onErr ma err = case ma of
  Just a -> Right a
  Nothing -> Left err

type Coords = { rowId :: Int, columnId :: Int }

type ParsedDescription =
  { atbCost :: Int
  , obLevel :: ObLevel
  , commandAbilitySigil :: Maybe Sigil
  }

-- #(ref:parse-cure-spell)
hasCureAllSAbility :: NonEmptyString -> Boolean
hasCureAllSAbility = NES.toString >>> String.startsWith "All (Cure Spells)"

parseDescription :: Coords -> Maybe NonEmptyString -> Maybe NonEmptyString -> NonEmptyString -> Result ParsedDescription
parseDescription coords heartCustomDescription spadeCustomDescription description = do
  let

    lines = description # NES.toString # String.lines
    firstLine = Arr.head lines `unsafeFromJust` "String.lines returned empty list"

    -- Parse weapon effects, discarding parser failures
    parseEffects :: Coords -> NonEmptyString -> Array WeaponEffect
    parseEffects coords desc =
      desc # NES.toString # String.lines
        # Arr.mapMaybe \line -> hush $ runParser line (parseWeaponEffect coords)

    effects =
      parseEffects coords description
        <> foldMap (parseEffects coords) heartCustomDescription
        <> foldMap (parseEffects coords) spadeCustomDescription
  -- Every weapon must have an ATB cost
  atbCost <- runParser firstLine (parseAtbCost coords) #
    lmap P.parseErrorMessage

  -- A weapon may have a C. Ability Sigil.
  let commandAbilitySigil = hush $ runParser firstLine (parseCommandAbilitySigil coords)

  pure
    { atbCost
    , commandAbilitySigil
    , obLevel:
        { description
        , heartCustomDescription
        , spadeCustomDescription
        , effects
        }
    }

parseAtbCost :: Coords -> Parser Int
parseAtbCost coords = do
  inContext ("Coords " <> show coords) do
    inContext "ATB Cost" do
      Tuple _ atbCost <- P.manyTill_ P.anyChar do
        P.char '(' *> P.intDecimal <* P.string " ATB)"
      pure atbCost

-- | Example input:
-- "Rolling Claw [Sigils: 1 \\◆] (3 ATB)"
parseCommandAbilitySigil :: Coords -> Parser Sigil
parseCommandAbilitySigil coords = do
  inContext ("Coords " <> show coords) do
    inContext "C. Ability Sigil" do
      Tuple _ sigil <- P.manyTill_ P.anyChar do
        P.string "[Sigils: " *> P.intDecimal *> P.string " \\" *> parseSigilSymbol <* P.string "]"
      pure sigil

-- | Example input:
-- "✖ Sigil Boost I"
--
-- #(ref:parse-sigil-boosts)
parseSAbilitySigilBoost :: Parser Sigil
parseSAbilitySigilBoost =
  parseSigilSymbol <* P.string " Sigil Boost"

parseSigilSymbol :: Parser Sigil
parseSigilSymbol =
  (P.char '◆' $> SigilDiamond)
    <|> ((P.char '⬤' <|> P.char '⏺') $> SigilO)
    <|> (P.char '✖' $> SigilX)
    <|> (P.char '▲' $> SigilTriangle)

space :: Parser Unit
space = void $ P.char ' '

-- E.g. `30s`
parseDuration :: Parser Duration
parseDuration = inContext "Duration" $ Duration <$> P.intDecimal <* P.char 's'

-- Some buffs (like Veil) have a percentage specifier
-- E.g. `30%`
parsePercentage :: Parser Percentage
parsePercentage = inContext "Percentage" $ Percentage <$> P.intDecimal <* P.char '%'

-- E.g. `(+30s)` or `(+12-20s)`
--
-- NOTE: "Crimson Blitz" seems to have a malformed "duration extension" in the sheet.
-- The in-game description shows `(+20s)`, but the spreadsheet says "(+12-20s)".
-- To account for this, we also parse "duration ranges", but then discard the 1st number and keep only the 2nd.
parseExtension :: Parser Extension
parseExtension =
  inContext "Extension" do
    Extension <$>
      inParens (P.char '+' *> (P.try parseRangeUpper <|> parseSingle))
  where
  -- Try parsing a range (for Crimson Blitz)
  parseRangeUpper = P.intDecimal *> P.char '-' *> P.intDecimal <* P.char 's'
  -- Try parsing a single number
  parseSingle = P.intDecimal <* P.char 's'

{-

Types of ranges:
  * [Range: All Allies]
  * [Range: Other Allies] (not currently handled)
  * [Range: Single Ally]
  * [Range: All Enemies]
  * [Range: Single Enemy]
  * [Range: Self]
-}
parseRange :: Parser Range
parseRange =
  inContext "Range" do
    inBrackets do
      _ <- P.string "Range: "
      (P.string "All Allies" <|> P.string "All Enemies") $> All
        <|> (P.string "Single Ally" <|> P.string "Single Enemy") $> SingleTarget
        <|> P.string "Self" $> Self

parsePotency :: Parser Potency
parsePotency =
  inContext "Potency" do
    P.string "Low" $> Low
      <|> P.string "Mid" $> Mid
      <|> P.string "High" $> High
      <|> P.string "Extra High" $> ExtraHigh

parsePotencies :: Parser { base :: Potency, max :: Potency }
parsePotencies =
  inContext "Potencies" do
    inParens $ P.try parseTwo <|> parseSingle
  where
  parseTwo =
    inContext "Potency Range" do
      base <- parsePotency
      _ <- P.string " -> "
      max <- parsePotency
      pure { base, max }

  parseSingle =
    inContext "Single Potency" do
      pot <- parsePotency
      pure { base: pot, max: pot }

-- Examples:
-- * `PATK Down (+7s) (Low -> Mid)`
-- * `Veil (+8s)`
-- * `Heal`
parseWeaponEffect :: Coords -> Parser WeaponEffect
parseWeaponEffect coords =
  inContext ("Coords " <> show coords) do
    inContext "WeaponEffect" do
      P.try (withPercentage "Heal" Heal)
      <|> P.try (withDurExtPotencies "PATK Up" PatkUp)
      <|> P.try (withDurExtPotencies "MATK Up" MatkUp)
      <|> P.try (withDurExtPotencies "PDEF Up" PdefUp)
      <|> P.try (withDurExtPotencies "MDEF Up" MdefUp)
      <|> P.try (withDurExtPercentage "Physical Weapon Boost" PhysicalWeaponBoost)
      <|> P.try (withDurExtPercentage "Magic Weapon Boost" MagicWeaponBoost)
      <|> P.try (withDurExtPercentage "Physical Damage Bonus" PhysicalDamageBonus)
      <|> P.try (withDurExtPercentage "Magic Damage Bonus" MagicDamageBonus)
      <|> P.try (withDurExtIgnoringSignedInt "ATB Phys. Weapon/Gear C. Ability Cost" PhysATBConservationEffect)
      <|> P.try (withDurExtIgnoringSignedInt "ATB Mag. Weapon/Gear C. Ability Cost" MagATBConservationEffect)
      <|> P.try (withDurExtPercentage "Amp. Phys. Abilities" AmpPhysAbilities)
      <|> P.try (withDurExtPercentage "Amp. Mag. Abilities" AmpMagAbilities)
      <|> P.try (withDurExtPotencies "Fire Damage Up" FireDamageUp)
      <|> P.try (withDurExtPotencies "Ice Damage Up" IceDamageUp)
      <|> P.try (withDurExtPotencies "Lightning Damage Up" LightningDamageUp)
      -- NOTE: the spreadsheet inconsistently names the lightning element either
      -- "Lightning" or "Thunder". We treat both spellings as the same effect.
      <|> P.try (withDurExtPotencies "Thunder Damage Up" LightningDamageUp)
      <|> P.try (withDurExtPotencies "Earth Damage Up" EarthDamageUp)
      <|> P.try (withDurExtPotencies "Water Damage Up" WaterDamageUp)
      <|> P.try (withDurExtPotencies "Wind Damage Up" WindDamageUp)
      <|> P.try (withDurExtPotencies "Fire Resistance Up" FireResistUp)
      <|> P.try (withDurExtPotencies "Ice Resistance Up" IceResistUp)
      <|> P.try (withDurExtPotencies "Lightning Resistance Up" LightningResistUp)
      <|> P.try (withDurExtPotencies "Thunder Resistance Up" LightningResistUp)
      <|> P.try (withDurExtPotencies "Earth Resistance Up" EarthResistUp)
      <|> P.try (withDurExtPotencies "Water Resistance Up" WaterResistUp)
      <|> P.try (withDurExtPotencies "Wind Resistance Up" WindResistUp)
      <|> P.try (withDurExtPercentage "Fire Weapon Boost" FireWeaponBoost)
      <|> P.try (withDurExtPercentage "Ice Weapon Boost" IceWeaponBoost)
      <|> P.try (withDurExtPercentage "Lightning Weapon Boost" LightningWeaponBoost)
      <|> P.try (withDurExtPercentage "Thunder Weapon Boost" LightningWeaponBoost)
      <|> P.try (withDurExtPercentage "Earth Weapon Boost" EarthWeaponBoost)
      <|> P.try (withDurExtPercentage "Water Weapon Boost" WaterWeaponBoost)
      <|> P.try (withDurExtPercentage "Wind Weapon Boost" WindWeaponBoost)
      <|> P.try (withDurExtPercentage "Fire Damage Bonus" FireDamageBonus)
      <|> P.try (withDurExtPercentage "Ice Damage Bonus" IceDamageBonus)
      <|> P.try (withDurExtPercentage "Lightning Damage Bonus" LightningDamageBonus)
      <|> P.try (withDurExtPercentage "Thunder Damage Bonus" LightningDamageBonus)
      <|> P.try (withDurExtPercentage "Earth Damage Bonus" EarthDamageBonus)
      <|> P.try (withDurExtPercentage "Water Damage Bonus" WaterDamageBonus)
      <|> P.try (withDurExtPercentage "Wind Damage Bonus" WindDamageBonus)
      <|> P.try (withDurExtIgnoringPercentage "Fire ATB Conservation Effect" FireATBConservationEffect)
      <|> P.try (withDurExtIgnoringSignedInt "ATB Fire Weapon/Gear C. Ability Cost" FireATBConservationEffect)
      <|> P.try (withDurExtIgnoringPercentage "Ice ATB Conservation Effect" IceATBConservationEffect)
      <|> P.try (withDurExtIgnoringSignedInt "ATB Ice Weapon/Gear C. Ability Cost" IceATBConservationEffect)
      <|> P.try (withDurExtIgnoringPercentage "Lightning ATB Conservation Effect" LightningATBConservationEffect)
      <|> P.try (withDurExtIgnoringSignedInt "ATB Lightning Weapon/Gear C. Ability Cost" LightningATBConservationEffect)
      <|> P.try (withDurExtIgnoringPercentage "Earth ATB Conservation Effect" EarthATBConservationEffect)
      <|> P.try (withDurExtIgnoringSignedInt "ATB Earth Weapon/Gear C. Ability Cost" EarthATBConservationEffect)
      <|> P.try (withDurExtIgnoringPercentage "Water ATB Conservation Effect" WaterATBConservationEffect)
      <|> P.try (withDurExtIgnoringSignedInt "ATB Water Weapon/Gear C. Ability Cost" WaterATBConservationEffect)
      <|> P.try (withDurExtIgnoringPercentage "Wind ATB Conservation Effect" WindATBConservationEffect)
      <|> P.try (withDurExtIgnoringSignedInt "ATB Wind Weapon/Gear C. Ability Cost" WindATBConservationEffect)
      <|> P.try (withDurExtIgnoringSignedInt "Amp. Fire Abilities" AmpFireAbilities)
      <|> P.try (withDurExtIgnoringSignedInt "Amp. Ice Abilities" AmpIceAbilities)
      <|> P.try (withDurExtIgnoringSignedInt "Amp. Lightning Abilities" AmpLightningAbilities)
      <|> P.try (withDurExtIgnoringSignedInt "Amp. Earth Abilities" AmpEarthAbilities)
      <|> P.try (withDurExtIgnoringSignedInt "Amp. Water Abilities" AmpWaterAbilities)
      <|> P.try (withDurExtIgnoringSignedInt "Amp. Wind Abilities" AmpWindAbilities)

      <|> P.try (withDurExtPercentage "Veil" Veil)
      <|> P.try (withDurExt "Provoke" Provoke)
      <|> P.try (withDurExtPotencies "PATK Down" PatkDown)
      <|> P.try (withDurExtPotencies "MATK Down" MatkDown)
      <|> P.try (withDurExtPotencies "PDEF Down" PdefDown)
      <|> P.try (withDurExtPotencies "MDEF Down" MdefDown)
      <|> P.try (withDurExtPercentage "Single-Tgt. Phys. Dmg. Rcvd. Up" SingleTgtFireDmgRcvdUp)
      <|> P.try (withDurExtPercentage "Single-Tgt. Mag. Dmg. Rcvd. Up" SingleTgtIceDmgRcvdUp)
      <|> P.try (withDurExtPercentage "All-Tgt. Phys. Dmg. Rcvd. Up" AllTgtFireDmgRcvdUp)
      <|> P.try (withDurExtPercentage "All-Tgt. Mag. Dmg. Rcvd. Up" AllTgtIceDmgRcvdUp)
      <|> P.try (withDurExtPotencies "Fire Damage Down" FireDamageDown)
      <|> P.try (withDurExtPotencies "Ice Damage Down" IceDamageDown)
      <|> P.try (withDurExtPotencies "Lightning Damage Down" LightningDamageDown)
      <|> P.try (withDurExtPotencies "Thunder Damage Down" LightningDamageDown)
      <|> P.try (withDurExtPotencies "Earth Damage Down" EarthDamageDown)
      <|> P.try (withDurExtPotencies "Water Damage Down" WaterDamageDown)
      <|> P.try (withDurExtPotencies "Wind Damage Down" WindDamageDown)
      <|> P.try (withDurExtPotencies "Fire Resistance Down" FireResistDown)
      <|> P.try (withDurExtPotencies "Ice Resistance Down" IceResistDown)
      <|> P.try (withDurExtPotencies "Lightning Resistance Down" LightningResistDown)
      <|> P.try (withDurExtPotencies "Thunder Resistance Down" LightningResistDown)
      <|> P.try (withDurExtPotencies "Earth Resistance Down" EarthResistDown)
      <|> P.try (withDurExtPotencies "Water Resistance Down" WaterResistDown)
      <|> P.try (withDurExtPotencies "Wind Resistance Down" WindResistDown)
      <|> P.try (withDurExtPercentage "Single-Tgt. Fire Dmg. Rcvd. Up" SingleTgtFireDmgRcvdUp)
      <|> P.try (withDurExtPercentage "Single-Tgt. Ice Dmg. Rcvd. Up" SingleTgtIceDmgRcvdUp)
      <|> P.try (withDurExtPercentage "Single-Tgt. Lightning Dmg. Rcvd. Up" SingleTgtLightningDmgRcvdUp)
      <|> P.try (withDurExtPercentage "Single-Tgt. Earth Dmg. Rcvd. Up" SingleTgtEarthDmgRcvdUp)
      <|> P.try (withDurExtPercentage "Single-Tgt. Water Dmg. Rcvd. Up" SingleTgtWaterDmgRcvdUp)
      <|> P.try (withDurExtPercentage "Single-Tgt. Wind Dmg. Rcvd. Up" SingleTgtWindDmgRcvdUp)
      <|> P.try (withDurExtPercentage "All-Tgt. Fire Dmg. Rcvd. Up" AllTgtFireDmgRcvdUp)
      <|> P.try (withDurExtPercentage "All-Tgt. Ice Dmg. Rcvd. Up" AllTgtIceDmgRcvdUp)
      <|> P.try (withDurExtPercentage "All-Tgt. Lightning Dmg. Rcvd. Up" AllTgtLightningDmgRcvdUp)
      <|> P.try (withDurExtPercentage "All-Tgt. Earth Dmg. Rcvd. Up" AllTgtEarthDmgRcvdUp)
      <|> P.try (withDurExtPercentage "All-Tgt. Water Dmg. Rcvd. Up" AllTgtWaterDmgRcvdUp)
      <|> P.try (withDurExtPercentage "All-Tgt. Wind Dmg. Rcvd. Up" AllTgtWindDmgRcvdUp)
      <|> P.try (withDurExtPercentage "Fire Weakness" FireWeakness)
      -- NOTE: "Gun of the Worthy" has a malformed "Ice Weakness" entry in the
      -- spreadsheet that omits the percentage. We fall back to 50% for that weapon.
      <|> P.try (withDurExtPercentage "Ice Weakness" IceWeakness)
      <|> P.try (withDurExtMissingPercentage (Percentage 50) "Ice Weakness" IceWeakness)
      <|> P.try (withDurExtPercentage "Lightning Weakness" LightningWeakness)
      <|> P.try (withDurExtPercentage "Thunder Weakness" LightningWeakness)
      <|> P.try (withDurExtPercentage "Earth Weakness" EarthWeakness)
      <|> P.try (withDurExtPercentage "Water Weakness" WaterWeakness)
      <|> P.try (withDurExtPercentage "Wind Weakness" WindWeakness)
      <|> P.try (withDurExt "Enfeeble" Enfeeble)
      <|> P.try (withDurExt "Stop" Stop)
      -- NOTE: the spreadsheet names this effect "Stance Gauge" (e.g. `+50% Stance Gauge`),
      -- but in-game it's the "Command Gauge", which is the name we use internally.
      <|> P.try (withPrefixedPercentage "Stance Gauge" IncreaseCommandGauge)
      <|> P.try (withDurExtPercentage "HP Gain" HPGain)
      <|> P.try (withExtPotencies "Enhance Buffs" EnhanceBuffs)
      <|> P.try (withExtPotencies "Enhance Debuffs" EnhanceDebuffs)
      <|> P.try (withDurExt "Enliven" Enliven)
      -- NOTE: The effect on "Blue Daffodil Gloves" is named "WeaknessAttackUp",
      -- but on "Bird of Prey" it's named "Exploit Weakness".
      <|> P.try (withDurExtPercentage "WeaknessAttackUp" ExploitWeakness)
      <|> P.try (withDurExtPercentage "Exploit Weakness" ExploitWeakness)
  where
  withPercentage :: String -> ({ range :: Range, percentage :: Percentage } -> WeaponEffect) -> Parser WeaponEffect
  withPercentage effectName constructor = do
    inContext effectName do
      percentage <- parsePercentage <* space
      _ <- P.string effectName <* space
      range <- parseRange
      pure $ constructor $ { range, percentage }

  -- Like `withPercentage`, but the percentage is prefixed with a `+` and the
  -- effect has no range in the game data, e.g. `+50% Stance Gauge [Condition: First Use]`.
  -- Any trailing text (like `[Condition: ...]`) is left unparsed and ignored.
  withPrefixedPercentage :: String -> ({ percentage :: Percentage } -> WeaponEffect) -> Parser WeaponEffect
  withPrefixedPercentage effectName constructor = do
    inContext effectName do
      _ <- P.char '+'
      percentage <- parsePercentage <* space
      _ <- P.string effectName
      pure $ constructor $ { percentage }

  withDurExt :: String -> ({ range :: Range, durExt :: DurExt } -> WeaponEffect) -> Parser WeaponEffect
  withDurExt effectName constructor = do
    inContext effectName do
      duration <- parseDuration <* space
      _ <- P.string effectName <* space
      extension <- parseExtension <* space
      range <- parseRange
      pure $ constructor $ { range, durExt: { duration, extension } }

  withDurExtPercentage :: String -> ({ range :: Range, durExt :: DurExt, percentage :: Percentage } -> WeaponEffect) -> Parser WeaponEffect
  withDurExtPercentage effectName constructor = do
    inContext effectName do
      duration <- parseDuration <* space
      percentage <- parsePercentage <* space
      _ <- P.string effectName <* space
      extension <- parseExtension <* space
      range <- parseRange
      pure $ constructor $ { range, durExt: { duration, extension }, percentage }

  -- Like `withDurExt`, but the game data includes a percentage between the duration and
  -- the effect name that we parse and discard. Handles both integer and fractional
  -- percentages, e.g. `25s 0% Phys ATB Conservation Effect (+8s) [Range: All Allies]`
  -- or `30s 0.2% Ice ATB Conservation Effect (+10s) [Range: Self]`.
  withDurExtIgnoringPercentage :: String -> ({ range :: Range, durExt :: DurExt } -> WeaponEffect) -> Parser WeaponEffect
  withDurExtIgnoringPercentage effectName constructor = do
    inContext effectName do
      duration <- parseDuration <* space
      _ <- (void $ P.intDecimal *> P.optionMaybe (P.char '.' *> P.intDecimal) *> P.char '%') <* space
      _ <- P.string effectName <* space
      extension <- parseExtension <* space
      range <- parseRange
      pure $ constructor { range, durExt: { duration, extension } }

  -- Like `withDurExt`, but the game data includes a signed integer between the duration
  -- and the effect name that we parse and discard.
  -- E.g. `10s -1 ATB Ice Weapon/Gear C. Ability Cost (+3s) [Range: All Allies]`
  withDurExtIgnoringSignedInt :: String -> ({ range :: Range, durExt :: DurExt } -> WeaponEffect) -> Parser WeaponEffect
  withDurExtIgnoringSignedInt effectName constructor = do
    inContext effectName do
      duration <- parseDuration <* space
      _ <- (void $ P.optionMaybe (P.char '-') *> P.intDecimal) <* space
      _ <- P.string effectName <* space
      extension <- parseExtension <* space
      range <- parseRange
      pure $ constructor { range, durExt: { duration, extension } }

  withDurExtMissingPercentage :: Percentage -> String -> ({ range :: Range, durExt :: DurExt, percentage :: Percentage } -> WeaponEffect) -> Parser WeaponEffect
  withDurExtMissingPercentage fallbackPercentage effectName constructor = do
    inContext effectName do
      duration <- parseDuration <* space
      _ <- P.string effectName <* space
      extension <- parseExtension <* space
      range <- parseRange
      pure $ constructor $ { range, durExt: { duration, extension }, percentage: fallbackPercentage }

  withDurExtPotencies :: String -> ({ range :: Range, durExt :: DurExt, potencies :: Potencies } -> WeaponEffect) -> Parser WeaponEffect
  withDurExtPotencies effectName constructor = do
    inContext effectName do
      duration <- parseDuration <* space
      _ <- P.string effectName <* space
      extension <- parseExtension <* space
      potencies <- parsePotencies <* space
      range <- parseRange
      pure $ constructor $ { range, durExt: { duration, extension }, potencies }

  -- Like `withDurExtPotencies`, but for effects that have no base duration in the
  -- game data, e.g. `Enhance Buffs (+10s) (Low -> Extra High) [Range: All Allies]`.
  -- These effects only extend the duration of *other* buffs/debuffs, so we record
  -- a duration of 0 (it's never displayed).
  withExtPotencies :: String -> ({ range :: Range, durExt :: DurExt, potencies :: Potencies } -> WeaponEffect) -> Parser WeaponEffect
  withExtPotencies effectName constructor = do
    inContext effectName do
      _ <- P.string effectName <* space
      extension <- parseExtension <* space
      potencies <- parsePotencies <* space
      range <- parseRange
      pure $ constructor $ { range, durExt: { duration: Duration 0, extension }, potencies }

inContext :: forall a. String -> Parser a -> Parser a
inContext context =
  P.region \(P.ParseError message pos) ->
    P.ParseError (context <> ": " <> message) pos

inParens :: forall a. Parser a -> Parser a
inParens = P.between (P.char '(') (P.char ')')

inBrackets :: forall a. Parser a -> Parser a
inBrackets = P.between (P.char '[') (P.char ']')
