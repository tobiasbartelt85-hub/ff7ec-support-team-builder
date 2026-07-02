module App.EffectSelector where

import Core.Database.Types
import Core.Database.UserState.VLatest
import Prelude

import App.WeaponModal as WeaponModal
import Core.Database.Types as Db
import Core.Display (display)
import Core.Weapons.Search (Filter, FilterRange, FilterResultWeapon, FilterResult)
import Core.Weapons.Search as Search
import Data.Array as Arr
import Data.Array.NonEmpty as NAR
import Data.Bounded.Generic (genericBottom)
import Data.Enum (succ)
import Data.Maybe (Maybe(..), isJust)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HtmlUtils (classes', displayIf, mkTooltipForWeapon, tooltip)
import Type.Prelude (Proxy(..))
import Utils (unsafeFromJust)
import Web.UIEvent.MouseEvent (MouseEvent)

type Slot id = H.Slot Query Output id

type Slots =
  ( weaponModal :: WeaponModal.Slot Unit
  )

_weaponModal = Proxy :: Proxy "weaponModal"

type Input =
  { dbState :: DbState
  , effectTypeMb :: Maybe FilterEffectType
  , canBeDeleted :: Boolean
  , enabledFilterEffectTypes :: Array FilterEffectType
  }

type State =
  { dbState :: DbState
  , selectedEffectType :: Maybe FilterEffectType
  , selectedRange :: FilterRange
  , selectedMinBasePotency :: Potency
  , selectedMinMaxPotency :: Potency
  , matchingWeapons :: Array FilterResultWeapon
  , canBeDeleted :: Boolean
  , weaponForModal :: Maybe WeaponModal.Input
  , rangeSuggestion :: Maybe FilterRange
  , enabledFilterEffectTypes :: Array FilterEffectType
  }

data Output
  = RaiseSelectionChanged
  | RaiseSetOwnedOb WeaponName Int
  | RaiseClosed

data Action
  = SelectedEffectType Int
  | SelectedRange Int
  | SelectedRange' FilterRange
  | SelectedMinBasePotency Int
  | SelectedMinMaxPotency Int
  | SetOwnedOb WeaponName Int
  | SelectedWeaponForModal WeaponModal.Input
  | HandleWeaponModal WeaponModal.Output
  | Initialize
  | Receive Input
  | Close MouseEvent

data Query a = GetFilter (Filter -> a)

component :: H.Component Query Input Output Aff
component =
  H.mkComponent
    { initialState: \{ dbState, effectTypeMb, canBeDeleted, enabledFilterEffectTypes } ->
        updateMatchingWeapons
          { dbState
          , selectedEffectType: effectTypeMb
          , selectedRange: genericBottom
          , selectedMinBasePotency: Low
          , selectedMinMaxPotency: High
          , matchingWeapons: []
          , canBeDeleted
          , weaponForModal: Nothing
          , rangeSuggestion: Nothing
          , enabledFilterEffectTypes
          }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

render :: State -> H.ComponentHTML Action Slots Aff
render state =
  HH.div [ classes' "columns is-mobile is-centered" ]
    -- Single column used to center the entire contents of the effect selector
    [ HH.div [ classes' "column is-narrow" ]
        -- Contains 2 columns: one for the 2 selects + another for the delete button
        -- vcentered so the delete button appears in line with the selects
        [ HH.div [ classes' "columns is-mobile is-centered is-vcentered" ]
            -- A column for the 2 selects
            [ HH.div [ classes' "column is-narrow" ]
                -- This `columns` is used to display the 2 selects side by side on wide screens
                -- and stacked vertically on mobile
                [ HH.div [ classes' "columns is-centered" ]
                    [ HH.div [ classes' "column is-narrow" ]
                        [ HH.div [ classes' "select" ]
                            [ HH.select
                                [ HE.onSelectedIndexChange SelectedEffectType
                                ]
                                ( [ HH.option_ [ HH.text "Select a weapon effect..." ] ]
                                    <>
                                      ( state.enabledFilterEffectTypes <#> \effectType -> do
                                          let selected = state.selectedEffectType == Just effectType
                                          HH.option [ HP.selected selected ] [ HH.text $ display effectType ]
                                      )
                                )
                            ]
                        ]
                    , displayIf (hasRange state.selectedEffectType) $
                        HH.div [ classes' "column is-narrow" ]
                          [ HH.div [ classes' "select" ]
                              [ HH.select
                                  [ HE.onSelectedIndexChange SelectedRange
                                  ]
                                  ( Search.allFilterRanges <#> \filterRange -> do
                                      let selected = state.selectedRange == filterRange
                                      HH.option [ HP.selected selected ] [ HH.text $ display filterRange ]
                                  )
                              ]
                          ]
                    ]
                ]

            -- A column for the delete button
            , HH.div [ classes' "column is-narrow" ]
                [ displayIf state.canBeDeleted $
                    HH.button [ classes' "delete is-medium", HE.onClick Close ] []
                ]
            ]

        -- Table for the potency filters
        , displayIf (hasPotencies state.selectedEffectType) $
            HH.div
              [ classes' "columns is-mobile is-centered is-vcentered is-1" ]
              [ HH.div [ classes' "column is-narrow" ]
                  [ HH.text "Base Pot. ≥"
                  ]
              , HH.div [ classes' "column is-narrow" ]
                  [ HH.div [ classes' "select" ]
                      [ HH.select
                          [ HE.onSelectedIndexChange SelectedMinBasePotency
                          ]
                          ( Db.allPossiblePotencies <#> \potency -> do
                              let selected = state.selectedMinBasePotency == potency
                              HH.option [ HP.selected selected ] [ HH.text $ display potency ]
                          )
                      ]
                  ]
              , HH.div [ classes' "column is-narrow" ]
                  [ HH.text "Max Pot. ≥"
                  ]
              , HH.div [ classes' "column is-narrow" ]
                  [ HH.div [ classes' "select" ]
                      [ HH.select
                          [ HE.onSelectedIndexChange SelectedMinMaxPotency
                          ]
                          ( Db.allPossiblePotencies <#> \potency -> do
                              let selected = state.selectedMinMaxPotency == potency
                              HH.option [ HP.selected selected ] [ HH.text $ display potency ]
                          )
                      ]
                  ]
              ]

        -- Used to center the table
        , HH.div [ classes' "columns is-mobile is-centered" ]
            [ HH.div [ classes' "column is-narrow" ]
                [ displayIf (Arr.null state.matchingWeapons && isJust state.selectedEffectType)
                    $ HH.div_
                    $
                      [ HH.p [ classes' "has-text-centered has-text-weight-semibold" ]
                          [ HH.text "No weapons found" ]
                      ]
                        <>
                          case state.rangeSuggestion of
                            Nothing -> []
                            Just rangeSuggestion ->
                              [ HH.text $ "Change range to "
                              , HH.a
                                  [ HE.onClick \_ -> SelectedRange' rangeSuggestion ]
                                  [ HH.text $ display rangeSuggestion ]
                              , HH.text "?"

                              ]

                , displayIf (not $ Arr.null state.matchingWeapons) $ HH.table [ classes' "table" ]
                    [ HH.tbody_ $
                        [ HH.tr_
                            [ HH.th_ []
                            , HH.th_ [ HH.text "Weapon" ]
                            , HH.th_ [ HH.text "Character" ]
                            , HH.th_ [ HH.text "Owned" ]
                            ]
                        ]
                          <>
                            ( state.matchingWeapons <#> \filterResultWeapon -> do
                                let weaponData = filterResultWeapon.weapon
                                let weaponState = filterResultWeapon.weaponState
                                let
                                  weaponModalInput =
                                    { weapon: weaponData.weapon
                                    , ownedOb: weaponState.ownedOb
                                    }

                                -- Grey out a row if the weapon does not match the filters
                                let
                                  checkCellDisabled classes =
                                    if not filterResultWeapon.matchesFilters then classes <> " has-text-primary-40"
                                    else classes
                                  checkRowDisabled classes =
                                    if not filterResultWeapon.matchesFilters then classes <> " has-background-primary-95"
                                    else classes
                                HH.tr
                                  [ classes' ("" # checkRowDisabled) ]
                                  [ HH.img
                                      [ HP.src (display weaponData.weapon.image)
                                      , classes' "is-clickable image is-32x32"
                                      , HE.onClick $ \_ -> SelectedWeaponForModal weaponModalInput
                                      ]
                                  , HH.td
                                      [ classes' ("is-clickable has-tooltip-right" # checkCellDisabled)
                                      , HE.onClick $ \_ -> SelectedWeaponForModal weaponModalInput
                                      , tooltip (mkTooltipForWeapon weaponData.weapon)
                                      ]
                                      [ HH.text $ display weaponData.weapon.name ]
                                  , HH.td
                                      [ classes' ("is-clickable has-tooltip-right" # checkCellDisabled)
                                      , HE.onClick $ \_ -> SelectedWeaponForModal weaponModalInput
                                      , tooltip (mkTooltipForWeapon weaponData.weapon)
                                      ]
                                      [ HH.text $ display weaponData.weapon.character ]
                                  , HH.td_
                                      [ HH.div [ classes' "select" ]
                                          [ HH.select
                                              [ HE.onSelectedIndexChange (SetOwnedOb weaponData.weapon.name) ]
                                              ( [ HH.option
                                                    [ HP.selected (weaponState.ownedOb == Nothing) ]
                                                    [ HH.text "N/A" ]
                                                ]
                                                  <>
                                                    ( NAR.toArray weaponData.distinctObs <#> \obRange ->
                                                        HH.option
                                                          [ HP.selected (weaponState.ownedOb == Just obRange) ]
                                                          [ HH.text $ display obRange ]
                                                    )
                                              )
                                          ]
                                      ]
                                  ]
                            )
                    ]
                ]
            ]
        ]
    , case state.weaponForModal of
        Nothing -> HH.div_ []
        Just weaponForModal -> HH.slot _weaponModal unit WeaponModal.component weaponForModal HandleWeaponModal
    ]

handleAction :: Action → H.HalogenM State Action Slots Output Aff Unit
handleAction = case _ of
  SelectedEffectType idx -> do
    if idx == 0 then
      do
        Console.log $ "Deselected effect type"
        H.modify_ \s -> s { selectedEffectType = Nothing }
          # updateMatchingWeapons
    else do
      state <- H.get
      -- Find the correct filter
      let arrayIndex = idx - 1
      let
        effectType = Arr.index state.enabledFilterEffectTypes arrayIndex `unsafeFromJust`
          ("Invalid effect type index: " <> show arrayIndex)

      Console.log $ "idx " <> show idx <> ", selected: " <> display effectType
      H.modify_ \s -> s { selectedEffectType = Just effectType }
        # updateMatchingWeapons
    H.raise RaiseSelectionChanged

  SelectedRange idx -> do
    let filterRange = Arr.index Search.allFilterRanges idx `unsafeFromJust` "Invalid filter range index"
    Console.log $ "idx " <> show idx <> ", selected: " <> display filterRange
    handleAction $ SelectedRange' filterRange

  SelectedRange' filterRange -> do
    H.modify_ \s -> s { selectedRange = filterRange }
      # updateMatchingWeapons
    H.raise RaiseSelectionChanged

  SelectedMinBasePotency idx -> do
    let minBasePotecy = Arr.index Db.allPossiblePotencies idx `unsafeFromJust` "Invalid base potency index"
    Console.log $ "idx " <> show idx <> ", selected: " <> display minBasePotecy
    H.modify_ \s -> s { selectedMinBasePotency = minBasePotecy }
      # updateMatchingWeapons
    H.raise RaiseSelectionChanged

  SelectedMinMaxPotency idx -> do
    let minMaxPotecy = Arr.index Db.allPossiblePotencies idx `unsafeFromJust` "Invalid max potency index"
    Console.log $ "idx " <> show idx <> ", selected: " <> display minMaxPotecy
    H.modify_ \s -> s { selectedMinMaxPotency = minMaxPotecy }
      # updateMatchingWeapons
    H.raise RaiseSelectionChanged

  SetOwnedOb weaponName obRangeIndex -> do
    H.raise $ RaiseSetOwnedOb weaponName obRangeIndex

  SelectedWeaponForModal weapon ->
    H.modify_ \s -> s { weaponForModal = Just weapon }

  HandleWeaponModal output ->
    case output of
      WeaponModal.ModalClosed ->
        H.modify_ \s -> s { weaponForModal = Nothing }

  Initialize -> do
    -- When this EffectSelector is done rendering, if the initial state has an effect type,
    -- we notify the root component so the results section will be updated.
    H.gets _.selectedEffectType >>= case _ of
      Just _ -> H.raise RaiseSelectionChanged
      Nothing -> pure unit

  Receive input -> do
    H.modify_ \state ->
      updateMatchingWeapons $ state
        { dbState = input.dbState
        , canBeDeleted = input.canBeDeleted
        }

  Close _ -> do
    H.raise RaiseClosed

updateMatchingWeapons :: State -> State
updateMatchingWeapons state = do
  case buildFilter state of
    Just filter -> do
      let filterResult = Search.findMatchingWeapons filter state.dbState
      let
        rangeSuggestion =
          if Arr.null filterResult.matchingWeapons then
            tryNextRange state.dbState filterResult
          else Nothing

      state { matchingWeapons = filterResult.matchingWeapons, rangeSuggestion = rangeSuggestion }
    Nothing -> state { matchingWeapons = [] }

tryNextRange :: DbState -> FilterResult -> Maybe FilterRange
tryNextRange dbState filterResult = do
  let filter = filterResult.filter
  case succ filter.range of
    Nothing -> Nothing
    Just nextRange -> do
      let nextFilter = filter { range = nextRange }
      let nextFilterResult = Search.findMatchingWeapons nextFilter dbState
      if Arr.null nextFilterResult.matchingWeapons then
        tryNextRange dbState nextFilterResult
      else
        Just nextRange

handleQuery :: forall action a m. Query a -> H.HalogenM State action Slots Output m (Maybe a)
handleQuery = case _ of
  GetFilter reply -> do
    state <- H.get
    case buildFilter state of
      Just filter -> pure $ Just $ reply filter
      Nothing -> pure Nothing

buildFilter :: State -> Maybe Filter
buildFilter state =
  case state.selectedEffectType of
    Nothing -> Nothing
    Just effectType -> Just
      { effectType
      , range: state.selectedRange
      , minBasePotency: state.selectedMinBasePotency
      , minMaxPotency: state.selectedMinMaxPotency
      }

hasPotencies :: Maybe FilterEffectType -> Boolean
hasPotencies = case _ of
  Nothing -> false
  Just effectType -> case effectType of
    FilterHeal -> false

    FilterVeil -> false
    FilterProvoke -> false
    FilterEnfeeble -> false
    FilterStop -> false
    FilterExploitWeakness -> false
    FilterIncreaseCommandGauge -> false
    FilterHPGain -> false
    FilterEnhanceBuffs -> true
    FilterEnhanceDebuffs -> true
    FilterEnliven -> false
    FilterPatkUp -> true
    FilterMatkUp -> true
    FilterPdefUp -> true
    FilterMdefUp -> true
    FilterPhysicalWeaponBoost -> false
    FilterMagicWeaponBoost -> false
    FilterPhysicalDamageBonus -> false
    FilterMagicDamageBonus -> false
    FilterPhysATBConservationEffect -> false
    FilterMagATBConservationEffect -> false
    FilterAmpPhysAbilities -> false
    FilterAmpMagAbilities -> false
    FilterFireDamageUp -> true
    FilterIceDamageUp -> true
    FilterLightningDamageUp -> true
    FilterEarthDamageUp -> true
    FilterWaterDamageUp -> true
    FilterWindDamageUp -> true
    FilterFireResistUp -> true
    FilterIceResistUp -> true
    FilterLightningResistUp -> true
    FilterEarthResistUp -> true
    FilterWaterResistUp -> true
    FilterWindResistUp -> true
    FilterFireWeaponBoost -> false
    FilterIceWeaponBoost -> false
    FilterLightningWeaponBoost -> false
    FilterEarthWeaponBoost -> false
    FilterWaterWeaponBoost -> false
    FilterWindWeaponBoost -> false
    FilterFireDamageBonus -> false
    FilterIceDamageBonus -> false
    FilterLightningDamageBonus -> false
    FilterEarthDamageBonus -> false
    FilterWaterDamageBonus -> false
    FilterWindDamageBonus -> false
    FilterFireATBConservationEffect -> false
    FilterIceATBConservationEffect -> false
    FilterLightningATBConservationEffect -> false
    FilterEarthATBConservationEffect -> false
    FilterWaterATBConservationEffect -> false
    FilterWindATBConservationEffect -> false
    FilterAmpFireAbilities -> false
    FilterAmpIceAbilities -> false
    FilterAmpLightningAbilities -> false
    FilterAmpEarthAbilities -> false
    FilterAmpWaterAbilities -> false
    FilterAmpWindAbilities -> false
    FilterPatkDown -> true
    FilterMatkDown -> true
    FilterPdefDown -> true
    FilterMdefDown -> true
    FilterSingleTgtPhysDmgRcvdUp -> false
    FilterSingleTgtMagDmgRcvdUp -> false
    FilterAllTgtPhysDmgRcvdUp -> false
    FilterAllTgtMagDmgRcvdUp -> false
    FilterFireDamageDown -> true
    FilterIceDamageDown -> true
    FilterLightningDamageDown -> true
    FilterEarthDamageDown -> true
    FilterWaterDamageDown -> true
    FilterWindDamageDown -> true
    FilterFireResistDown -> true
    FilterIceResistDown -> true
    FilterLightningResistDown -> true
    FilterEarthResistDown -> true
    FilterWaterResistDown -> true
    FilterWindResistDown -> true
    FilterSingleTgtFireDmgRcvdUp -> false
    FilterSingleTgtIceDmgRcvdUp -> false
    FilterSingleTgtLightningDmgRcvdUp -> false
    FilterSingleTgtEarthDmgRcvdUp -> false
    FilterSingleTgtWaterDmgRcvdUp -> false
    FilterSingleTgtWindDmgRcvdUp -> false
    FilterAllTgtFireDmgRcvdUp -> false
    FilterAllTgtIceDmgRcvdUp -> false
    FilterAllTgtLightningDmgRcvdUp -> false
    FilterAllTgtEarthDmgRcvdUp -> false
    FilterAllTgtWaterDmgRcvdUp -> false
    FilterAllTgtWindDmgRcvdUp -> false
    FilterFireWeakness -> false
    FilterIceWeakness -> false
    FilterLightningWeakness -> false
    FilterEarthWeakness -> false
    FilterWaterWeakness -> false
    FilterWindWeakness -> false

    FilterSigilBoostO -> false
    FilterSigilBoostX -> false
    FilterSigilBoostTriangle -> false
    FilterSigilDiamond -> false

hasRange :: Maybe FilterEffectType -> Boolean
hasRange = case _ of
  Nothing -> false
  Just effectType -> case effectType of
    FilterHeal -> true

    FilterVeil -> true
    FilterProvoke -> true
    FilterEnfeeble -> true
    FilterStop -> true
    FilterExploitWeakness -> true
    FilterIncreaseCommandGauge -> false
    FilterHPGain -> true
    -- This effect has no range in the game data (its weapons are grouped with
    -- `ranges: Nothing`), so there's no range selector to show.

    FilterEnhanceBuffs -> true
    FilterEnhanceDebuffs -> true
    FilterEnliven -> true

    FilterPatkUp -> true
    FilterMatkUp -> true
    FilterPdefUp -> true
    FilterMdefUp -> true
    FilterSingleTgtPhysDmgRcvdUp -> false
    FilterSingleTgtMagDmgRcvdUp -> false
    FilterAllTgtPhysDmgRcvdUp -> false
    FilterAllTgtMagDmgRcvdUp -> false
    FilterPhysicalWeaponBoost -> true
    FilterMagicWeaponBoost -> true
    FilterPhysicalDamageBonus -> true
    FilterMagicDamageBonus -> true
    FilterPhysATBConservationEffect -> false
    FilterMagATBConservationEffect -> false
    FilterAmpPhysAbilities -> false
    FilterAmpMagAbilities -> false
    FilterFireDamageUp -> true
    FilterIceDamageUp -> true
    FilterLightningDamageUp -> true
    FilterEarthDamageUp -> true
    FilterWaterDamageUp -> true
    FilterWindDamageUp -> true
    FilterFireResistUp -> true
    FilterIceResistUp -> true
    FilterLightningResistUp -> true
    FilterEarthResistUp -> true
    FilterWaterResistUp -> true
    FilterWindResistUp -> true
    FilterFireWeaponBoost -> true
    FilterIceWeaponBoost -> true
    FilterLightningWeaponBoost -> true
    FilterEarthWeaponBoost -> true
    FilterWaterWeaponBoost -> true
    FilterWindWeaponBoost -> true
    FilterFireDamageBonus -> true
    FilterIceDamageBonus -> true
    FilterLightningDamageBonus -> true
    FilterEarthDamageBonus -> true
    FilterWaterDamageBonus -> true
    FilterWindDamageBonus -> true
    FilterFireATBConservationEffect -> false
    FilterIceATBConservationEffect -> false
    FilterLightningATBConservationEffect -> false
    FilterEarthATBConservationEffect -> false
    FilterWaterATBConservationEffect -> false
    FilterWindATBConservationEffect -> false
    FilterAmpFireAbilities -> false
    FilterAmpIceAbilities -> false
    FilterAmpLightningAbilities -> false
    FilterAmpEarthAbilities -> false
    FilterAmpWaterAbilities -> false
    FilterAmpWindAbilities -> false

    FilterPatkDown -> true
    FilterMatkDown -> true
    FilterPdefDown -> true
    FilterMdefDown -> true
    FilterFireDamageDown -> true
    FilterIceDamageDown -> true
    FilterLightningDamageDown -> true
    FilterEarthDamageDown -> true
    FilterWaterDamageDown -> true
    FilterWindDamageDown -> true
    FilterFireResistDown -> true
    FilterIceResistDown -> true
    FilterLightningResistDown -> true
    FilterEarthResistDown -> true
    FilterWaterResistDown -> true
    FilterWindResistDown -> true
    FilterSingleTgtFireDmgRcvdUp -> false
    FilterSingleTgtIceDmgRcvdUp -> false
    FilterSingleTgtLightningDmgRcvdUp -> false
    FilterSingleTgtEarthDmgRcvdUp -> false
    FilterSingleTgtWaterDmgRcvdUp -> false
    FilterSingleTgtWindDmgRcvdUp -> false
    FilterAllTgtFireDmgRcvdUp -> false
    FilterAllTgtIceDmgRcvdUp -> false
    FilterAllTgtLightningDmgRcvdUp -> false
    FilterAllTgtEarthDmgRcvdUp -> false
    FilterAllTgtWaterDmgRcvdUp -> false
    FilterAllTgtWindDmgRcvdUp -> false
    FilterFireWeakness -> true
    FilterIceWeakness -> true
    FilterLightningWeakness -> true
    FilterEarthWeakness -> true
    FilterWaterWeakness -> true
    FilterWindWeakness -> true

    FilterSigilBoostO -> false
    FilterSigilBoostX -> false
    FilterSigilBoostTriangle -> false
    FilterSigilDiamond -> false
