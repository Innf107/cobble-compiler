module Language.Cobble.McFunction.Types where

import Language.Cobble.Prelude

import Language.Cobble.MCAsm.Types

import qualified Data.Text as T

import qualified GHC.Show as S
import Language.Cobble.Shared

data Bloc = Block QualifiedName [Command] deriving (Show, Eq, Generic, Data)

type Objective = Text

type NamespacedName = QualifiedName -- TODO

data NBT --TODO
    deriving (Show, Eq, Generic, Data)

data Position --TODO
    deriving (Show, Eq, Generic, Data) 

data Command = Advancement Void
             | Attribute Void
             | Bossbar Void
             | Clear Void
             | Clone Void
             | Data Void
             | Datapack Void
             | Debug Void
             | DefaultGamemode Gamemode
             | Difficulty Difficulty
             | Effect Void
             | Enchant Selector NamespacedName (Maybe Int)
             | Execute Void -- TODO!
             | Experience Void
             | Fill Void
             | Forceload Void
             | Function NamespacedName
             | Gamemode Gamemode (Maybe Selector)
             | Gamerule Void
             | Give Selector NamespacedName (Maybe Int)
             | Help
             | Item Void
             | Kick Selector (Maybe Text)
             | Kill Selector
             | List
             | ListUUIDs
             | Locate Text
             | LocateBiome NamespacedName
             | Loot Void
             | Me Text
             | Msg Selector Text
             | Particle Void
             | Playsound Void
             | Publish
             | Recipe Void
             | Reload
             | Say Text
             | Schedule ScheduleArg NamespacedName
             | Scoreboard ScoreboardArg
             | Seed
             | Setblock Void
             | Setworldspawn Void
             | Spawnpoint Void
             | Spectate Void
             | Spreadplayers Void
             | StopSound Void
             | Summon NamespacedName (Maybe SummonArg)
             | Tag Void
             | Team Void
             | TeamMsg Text
             | Teleport Void
             | Tell Selector Text
             | TellRaw Selector Text
             | Time Void
             | Title Void
             | TM Text
             | Tp Void
             | Trigger Void
             | Weather Weather
             | Worldboarder Void
             | Xp Void
             deriving (Show, Eq, Generic, Data)

data ScoreboardArg = Players PlayerScoreboardArg
                   | Objectives ObjectiveScoreboardArg
                   deriving (Show, Eq, Generic, Data)

data PlayerScoreboardArg = Add Selector Objective Int
                         | Enable Selector Objective
                         | Get Selector Objective
                         | SList (Maybe Selector)
                         | Operation Selector Objective SOperation Selector Objective
                         | Remove Selector Objective Int
                         | Reset Selector (Maybe Objective)
                         | Set Selector Objective Int 
                         deriving (Show, Eq, Generic, Data)

data SOperation = SMod 
                | SMul
                | SAdd
                | SSub
                | SDiv
                | SMin
                | SAssign
                | SMax
                | SSwap
                deriving (Show, Eq, Generic, Data)

data ObjectiveScoreboardArg = OAdd Objective Text (Maybe Text)
                            | ORemove Objective
                            | OModify OModifyArg
                            | OList
                            | OSetDisplay Objective Text 
                            deriving (Show, Eq, Generic, Data)

data OModifyArg = DisplayName Text
                | RenderType OModifyRenderType 
                deriving (Show, Eq, Generic, Data)

data OModifyRenderType = RHearts | RIntegers deriving (Show, Eq, Generic, Data)

data Selector = Player Text
              | Entity [SelectorArg]
              | AllPlayers [SelectorArg]
              | NearestPlayer [SelectorArg]
              | RandomPlayer [SelectorArg]
              | Self [SelectorArg]
              deriving (Show, Eq, Generic, Data)

data SelectorArg deriving (Show, Eq, Generic, Data)

data Gamemode = Survival | Creative | Adventure | Spectator deriving (Show, Eq, Generic, Data, Enum)

data Difficulty = Peaceful | Easy | Normal | Hard deriving (Show, Eq, Generic, Data, Ord, Enum)

data ScheduleArg = SClear | SFunction deriving (Show, Eq, Generic, Data)

data Weather = ClearW | Rain | Thunder deriving (Show, Eq, Generic, Data)

data SummonArg = SummonArg Position (Maybe NBT) deriving (Show, Eq, Generic, Data)

