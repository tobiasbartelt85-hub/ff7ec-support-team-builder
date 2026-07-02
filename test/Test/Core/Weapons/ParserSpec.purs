module Test.Core.Weapons.ParserSpec where

import Prelude
import Test.Spec

import Control.Monad.Error.Class (throwError)
import Core.Weapons.Parser
import Core.Database.Types
import Data.Either (Either(..))
import Effect.Aff (error)
import Google.SheetsApi (GetSheetResult)
import Node.Encoding as Node
import Node.FS.Aff as Node
import Test.Utils as T
import Utils as Utils
import Yoga.JSON as J

spec :: Spec Unit
spec =
  describe "parser" do
    it "parses weapon effects" do
      let
        shouldParse = T.shouldParse' (parseWeaponEffect { rowId: 0, columnId: 0 })
      "60s Provoke (+0s) [Range: Self]"
        `shouldParse`
          Provoke { range: Self, durExt: { duration: Duration 60, extension: Extension 0 } }
      "40s 5% Veil (+8s) [Range: Self]"
        `shouldParse`
          Veil { range: Self, durExt: { duration: Duration 40, extension: Extension 8 }, percentage: Percentage 5 }
      "74% Heal [Range: All Allies]"
        `shouldParse`
          Heal { range: All, percentage: Percentage 74 }
      "16s PATK Up (+5s) (Mid -> High) [Range: All Allies]"
        `shouldParse`
          PatkUp { range: All, durExt: { duration: Duration 16, extension: Extension 5 }, potencies: { base: Mid, max: High } }
      "20s PATK Up (+6s) (High) [Range: All Allies]"
        `shouldParse`
          PatkUp { range: All, durExt: { duration: Duration 20, extension: Extension 6 }, potencies: { base: High, max: High } }
      "20s MDEF Up (+6s) (High) [Range: All Allies] [Condition: Self 70-100% HP]"
        `shouldParse`
          MdefUp { range: All, durExt: { duration: Duration 20, extension: Extension 6 }, potencies: { base: High, max: High } }
      "26s Ice Damage Up (+8s) (Mid) [Range: Single Ally]"
        `shouldParse`
          IceDamageUp { range: SingleTarget, durExt: { duration: Duration 26, extension: Extension 8 }, potencies: { base: Mid, max: Mid } }
      "3s Stop (+3s) [Range: Single Enemy] [Condition: First Use]"
        `shouldParse`
          Stop { range: SingleTarget, durExt: { duration: Duration 3, extension: Extension 3 } }
      "40s Enfeeble (+8s) [Range: Single Enemy]"
        `shouldParse`
          Enfeeble { range: SingleTarget, durExt: { duration: Duration 40, extension: Extension 8 } }
      "45s 25% WeaknessAttackUp (+9s) [Range: Self]"
        `shouldParse`
          ExploitWeakness { range: Self, durExt: { duration: Duration 45, extension: Extension 9 }, percentage: Percentage 25 }
      "45s 30% Exploit Weakness (+9s) [Range: Self]"
        `shouldParse`
          ExploitWeakness { range: Self, durExt: { duration: Duration 45, extension: Extension 9 }, percentage: Percentage 30 }
      "30s Enliven (+10s) [Range: Self]"
        `shouldParse`
          Enliven { range: Self, durExt: { duration: Duration 30, extension: Extension 10 } }
      "30s 4% HP Gain (+0s) [Range: All Allies]"
        `shouldParse`
          HPGain { range: All, durExt: { duration: Duration 30, extension: Extension 0 }, percentage: Percentage 4 }
      "+50% Stance Gauge [Condition: First Use]"
        `shouldParse`
          IncreaseCommandGauge { percentage: Percentage 50 }
      "+10% Stance Gauge"
        `shouldParse`
          IncreaseCommandGauge { percentage: Percentage 10 }
      "Enhance Buffs (+10s) (Low -> Extra High) [Range: All Allies]"
        `shouldParse`
          EnhanceBuffs { range: All, durExt: { duration: Duration 0, extension: Extension 10 }, potencies: { base: Low, max: ExtraHigh } }
      "Enhance Debuffs (+10s) (Low -> High) [Range: All Enemies]"
        `shouldParse`
          EnhanceDebuffs { range: All, durExt: { duration: Duration 0, extension: Extension 10 }, potencies: { base: Low, max: High } }
      "150s Ice Weakness (+0s) [Range: Single Enemy] [Condition: First Use]"
        `shouldParse`
          IceWeakness { range: SingleTarget, durExt: { duration: Duration 150, extension: Extension 0 }, percentage: Percentage 50 }
      "30s 25% Physical Weapon Boost (+10s) [Range: Self]"
        `shouldParse`
          PhysicalWeaponBoost { range: Self, durExt: { duration: Duration 30, extension: Extension 10 }, percentage: Percentage 25 }
      "30s 20% Magic Weapon Boost (+10s) [Range: All Allies] [Condition: Self 50-100% HP]"
        `shouldParse`
          MagicWeaponBoost { range: All, durExt: { duration: Duration 30, extension: Extension 10 }, percentage: Percentage 20 }
      "20s 20% Physical Damage Bonus (+6s) [Range: All Allies]"
        `shouldParse`
          PhysicalDamageBonus { range: All, durExt: { duration: Duration 20, extension: Extension 6 }, percentage: Percentage 20 }
      "30s 20% Magic Damage Bonus (+10s) [Range: All Allies]"
        `shouldParse`
          MagicDamageBonus { range: All, durExt: { duration: Duration 30, extension: Extension 10 }, percentage: Percentage 20 }
      "25s -1 ATB Phys. Weapon/Gear C. Ability Cost (+8s) [Range: All Allies]"
        `shouldParse`
          PhysATBConservationEffect { range: All, durExt: { duration: Duration 25, extension: Extension 8 } }
      "25s -1 ATB Mag. Weapon/Gear C. Ability Cost (+8s) [Range: All Allies]"
        `shouldParse`
          MagATBConservationEffect { range: All, durExt: { duration: Duration 25, extension: Extension 8 } }
      "30s 25% Fire Weapon Boost (+10s) [Range: All Allies]"
        `shouldParse`
          FireWeaponBoost { range: All, durExt: { duration: Duration 30, extension: Extension 10 }, percentage: Percentage 25 }
      "15s 20% Ice Weapon Boost (+5s) [Range: All Allies]"
        `shouldParse`
          IceWeaponBoost { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, percentage: Percentage 20 }
      "30s 20% Lightning Weapon Boost (+10s) [Range: All Allies]"
        `shouldParse`
          LightningWeaponBoost { range: All, durExt: { duration: Duration 30, extension: Extension 10 }, percentage: Percentage 20 }
      "30s 25% Earth Weapon Boost (+10s) [Range: All Allies]"
        `shouldParse`
          EarthWeaponBoost { range: All, durExt: { duration: Duration 30, extension: Extension 10 }, percentage: Percentage 25 }
      "30s 20% Water Weapon Boost (+10s) [Range: All Allies]"
        `shouldParse`
          WaterWeaponBoost { range: All, durExt: { duration: Duration 30, extension: Extension 10 }, percentage: Percentage 20 }
      "30s 25% Wind Weapon Boost (+10s) [Range: All Allies]"
        `shouldParse`
          WindWeaponBoost { range: All, durExt: { duration: Duration 30, extension: Extension 10 }, percentage: Percentage 25 }
      "30s 20% Fire Damage Bonus (+10s) [Range: All Allies]"
        `shouldParse`
          FireDamageBonus { range: All, durExt: { duration: Duration 30, extension: Extension 10 }, percentage: Percentage 20 }
      "30s 20% Ice Damage Bonus (+10s) [Range: All Allies]"
        `shouldParse`
          IceDamageBonus { range: All, durExt: { duration: Duration 30, extension: Extension 10 }, percentage: Percentage 20 }
      "15s 20% Lightning Damage Bonus (+5s) [Range: All Allies]"
        `shouldParse`
          LightningDamageBonus { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, percentage: Percentage 20 }
      "15s 20% Earth Damage Bonus (+5s) [Range: All Allies]"
        `shouldParse`
          EarthDamageBonus { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, percentage: Percentage 20 }
      "30s 20% Water Damage Bonus (+10s) [Range: All Allies]"
        `shouldParse`
          WaterDamageBonus { range: All, durExt: { duration: Duration 30, extension: Extension 10 }, percentage: Percentage 20 }
      "15s 20% Wind Damage Bonus (+5s) [Range: All Allies]"
        `shouldParse`
          WindDamageBonus { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, percentage: Percentage 20 }
      "15s 20% Single-Tgt. Phys. Dmg. Rcvd. Up (+5s) [Range: All Allies]"
        `shouldParse`
          SingleTgtPhysDmgRcvdUp { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, percentage: Percentage 20 }
      "15s 20% Single-Tgt. Mag. Dmg. Rcvd. Up (+5s) [Range: All Allies]"
        `shouldParse`
          SingleTgtMagDmgRcvdUp { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, percentage: Percentage 20 }
      "15s 20% All-Tgt. Phys. Dmg. Rcvd. Up (+5s) [Range: All Allies]"
        `shouldParse`
          AllTgtPhysDmgRcvdUp { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, percentage: Percentage 20 }
      "15s 20% All-Tgt. Mag. Dmg. Rcvd. Up (+5s) [Range: All Allies]"
        `shouldParse`
          AllTgtMagDmgRcvdUp { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, percentage: Percentage 20 }
      "15s 20% Single-Tgt. Fire Dmg. Rcvd. Up (+5s) [Range: All Allies]"
        `shouldParse`
          SingleTgtFireDmgRcvdUp { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, percentage: Percentage 20 }
      "15s 20% Single-Tgt. Ice Dmg. Rcvd. Up (+5s) [Range: All Allies]"
        `shouldParse`
          SingleTgtIceDmgRcvdUp { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, percentage: Percentage 20 }
      "15s 20% Single-Tgt. Lightning Dmg. Rcvd. Up (+5s) [Range: All Allies]"
        `shouldParse`
          SingleTgtLightningDmgRcvdUp { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, percentage: Percentage 20 }
      "15s 20% Single-Tgt. Earth Dmg. Rcvd. Up (+5s) [Range: All Allies]"
        `shouldParse`
          SingleTgtEarthDmgRcvdUp { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, percentage: Percentage 20 }
      "15s 20% Single-Tgt. Water Dmg. Rcvd. Up (+5s) [Range: All Allies]"
        `shouldParse`
          SingleTgtWaterDmgRcvdUp { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, percentage: Percentage 20 }
      "15s 20% Single-Tgt. Wind Dmg. Rcvd. Up (+5s) [Range: All Allies]"
        `shouldParse`
          SingleTgtWindDmgRcvdUp { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, percentage: Percentage 20 }
      "15s 20% All-Tgt. Fire Dmg. Rcvd. Up (+5s) [Range: All Allies]"
        `shouldParse`
          AllTgtFireDmgRcvdUp { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, percentage: Percentage 20 }
      "15s 20% All-Tgt. Ice Dmg. Rcvd. Up (+5s) [Range: All Allies]"
        `shouldParse`
          AllTgtIceDmgRcvdUp { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, percentage: Percentage 20 }
      "15s 20% All-Tgt. Lightning Dmg. Rcvd. Up (+5s) [Range: All Allies]"
        `shouldParse`
          AllTgtLightningDmgRcvdUp { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, percentage: Percentage 20 }
      "15s 20% All-Tgt. Earth Dmg. Rcvd. Up (+5s) [Range: All Allies]"
        `shouldParse`
          AllTgtEarthDmgRcvdUp { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, percentage: Percentage 20 }
      "15s 20% All-Tgt. Water Dmg. Rcvd. Up (+5s) [Range: All Allies]"
        `shouldParse`
          AllTgtWaterDmgRcvdUp { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, percentage: Percentage 20 }
      "15s 20% All-Tgt. Wind Dmg. Rcvd. Up (+5s) [Range: All Allies]"
        `shouldParse`
          AllTgtWindDmgRcvdUp { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, percentage: Percentage 20 }
      "15s 20% Amp. Phys. Abilities (+5s) [Range: All Allies]"
        `shouldParse`
          AmpPhysAbilities { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, percentage: Percentage 20 }
      "15s 20% Amp. Mag. Abilities (+5s) [Range: All Allies]"
        `shouldParse`
          AmpMagAbilities { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, percentage: Percentage 20 }
      "15s 20% Amp. Fire Abilities (+5s) [Range: All Allies]"
        `shouldParse`
          AmpFireAbilities { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, percentage: Percentage 20 }
      "15s 20% Amp. Ice Abilities (+5s) [Range: All Allies]"
        `shouldParse`
          AmpIceAbilities { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, percentage: Percentage 20 }
      "15s 20% Amp. Lightning Abilities (+5s) [Range: All Allies]"
        `shouldParse`
          AmpLightningAbilities { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, percentage: Percentage 20 }
      "15s 20% Amp. Earth Abilities (+5s) [Range: All Allies]"
        `shouldParse`
          AmpEarthAbilities { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, percentage: Percentage 20 }
      "15s 20% Amp. Water Abilities (+5s) [Range: All Allies]"
        `shouldParse`
          AmpWaterAbilities { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, percentage: Percentage 20 }
      "15s 20% Amp. Wind Abilities (+5s) [Range: All Allies]"
        `shouldParse`
          AmpWindAbilities { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, percentage: Percentage 20 }
      "25s 0% Fire ATB Conservation Effect (+8s) [Range: All Allies]"
        `shouldParse`
          FireATBConservationEffect { range: All, durExt: { duration: Duration 25, extension: Extension 8 } }
      "25s -1 ATB Fire Weapon/Gear C. Ability Cost (+8s) [Range: All Allies]"
        `shouldParse`
          FireATBConservationEffect { range: All, durExt: { duration: Duration 25, extension: Extension 8 } }
      "25s 0% Ice ATB Conservation Effect (+8s) [Range: All Allies]"
        `shouldParse`
          IceATBConservationEffect { range: All, durExt: { duration: Duration 25, extension: Extension 8 } }
      "30s 0.2% Ice ATB Conservation Effect (+10s) [Range: Self] [Condition: First Use]"
        `shouldParse`
          IceATBConservationEffect { range: Self, durExt: { duration: Duration 30, extension: Extension 10 } }
      "25s -1 ATB Ice Weapon/Gear C. Ability Cost (+8s) [Range: All Allies]"
        `shouldParse`
          IceATBConservationEffect { range: All, durExt: { duration: Duration 25, extension: Extension 8 } }
      "25s 0% Lightning ATB Conservation Effect (+8s) [Range: All Allies]"
        `shouldParse`
          LightningATBConservationEffect { range: All, durExt: { duration: Duration 25, extension: Extension 8 } }
      "25s -1 ATB Lightning Weapon/Gear C. Ability Cost (+8s) [Range: All Allies]"
        `shouldParse`
          LightningATBConservationEffect { range: All, durExt: { duration: Duration 25, extension: Extension 8 } }
      "25s 0% Earth ATB Conservation Effect (+8s) [Range: All Allies]"
        `shouldParse`
          EarthATBConservationEffect { range: All, durExt: { duration: Duration 25, extension: Extension 8 } }
      "25s -1 ATB Earth Weapon/Gear C. Ability Cost (+8s) [Range: All Allies]"
        `shouldParse`
          EarthATBConservationEffect { range: All, durExt: { duration: Duration 25, extension: Extension 8 } }
      "25s 0% Water ATB Conservation Effect (+8s) [Range: All Allies]"
        `shouldParse`
          WaterATBConservationEffect { range: All, durExt: { duration: Duration 25, extension: Extension 8 } }
      "25s -1 ATB Water Weapon/Gear C. Ability Cost (+8s) [Range: All Allies]"
        `shouldParse`
          WaterATBConservationEffect { range: All, durExt: { duration: Duration 25, extension: Extension 8 } }
      "25s 0% Wind ATB Conservation Effect (+8s) [Range: All Allies]"
        `shouldParse`
          WindATBConservationEffect { range: All, durExt: { duration: Duration 25, extension: Extension 8 } }
      "25s -1 ATB Wind Weapon/Gear C. Ability Cost (+8s) [Range: All Allies]"
        `shouldParse`
          WindATBConservationEffect { range: All, durExt: { duration: Duration 25, extension: Extension 8 } }
      "15s Fire Damage Down (+5s) (Mid -> High) [Range: All Allies]"
        `shouldParse`
          FireDamageDown { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, potencies: { base: Mid, max: High } }
      "15s Ice Damage Down (+5s) (Mid -> High) [Range: All Allies]"
        `shouldParse`
          IceDamageDown { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, potencies: { base: Mid, max: High } }
      "15s Lightning Damage Down (+5s) (Mid -> High) [Range: All Allies]"
        `shouldParse`
          LightningDamageDown { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, potencies: { base: Mid, max: High } }
      "15s Earth Damage Down (+5s) (Mid -> High) [Range: All Allies]"
        `shouldParse`
          EarthDamageDown { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, potencies: { base: Mid, max: High } }
      "15s Water Damage Down (+5s) (Mid -> High) [Range: All Allies]"
        `shouldParse`
          WaterDamageDown { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, potencies: { base: Mid, max: High } }
      "15s Wind Damage Down (+5s) (Mid -> High) [Range: All Allies]"
        `shouldParse`
          WindDamageDown { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, potencies: { base: Mid, max: High } }
      "15s Fire Resistance Up (+5s) (Mid -> High) [Range: All Allies]"
        `shouldParse`
          FireResistUp { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, potencies: { base: Mid, max: High } }
      "15s Ice Resistance Up (+5s) (Mid -> High) [Range: All Allies]"
        `shouldParse`
          IceResistUp { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, potencies: { base: Mid, max: High } }
      "15s Lightning Resistance Up (+5s) (Mid -> High) [Range: All Allies]"
        `shouldParse`
          LightningResistUp { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, potencies: { base: Mid, max: High } }
      "15s Earth Resistance Up (+5s) (Mid -> High) [Range: All Allies]"
        `shouldParse`
          EarthResistUp { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, potencies: { base: Mid, max: High } }
      "15s Water Resistance Up (+5s) (Mid -> High) [Range: All Allies]"
        `shouldParse`
          WaterResistUp { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, potencies: { base: Mid, max: High } }
      "15s Wind Resistance Up (+5s) (Mid -> High) [Range: All Allies]"
        `shouldParse`
          WindResistUp { range: All, durExt: { duration: Duration 15, extension: Extension 5 }, potencies: { base: Mid, max: High } }
      -- The spreadsheet uses "Thunder" and "Lightning" interchangeably for the
      -- lightning element; both spellings must map to the same effect.
      "16s Thunder Damage Up (+5s) (Mid -> High) [Range: All Allies]"
        `shouldParse`
          LightningDamageUp { range: All, durExt: { duration: Duration 16, extension: Extension 5 }, potencies: { base: Mid, max: High } }
      "20s Thunder Resistance Down (+6s) (Low -> Mid) [Range: Single Enemy]"
        `shouldParse`
          LightningResistDown { range: SingleTarget, durExt: { duration: Duration 20, extension: Extension 6 }, potencies: { base: Low, max: Mid } }
    it "parses all weapons" do
      sourceWeaponsJson <- Node.readTextFile Node.UTF8 "resources/weapons.json"
      sourceWeapons <- case J.readJSON sourceWeaponsJson :: _ GetSheetResult of
        Right res -> pure res.values
        Left errs ->
          throwError $ error
            $ "Failed to read `resources/weapons.json`: \n"
                <> Utils.renderJsonErr errs
      let parseResult = parseWeapons sourceWeapons
      T.goldenTest "snaps/weapons.snap" parseResult
