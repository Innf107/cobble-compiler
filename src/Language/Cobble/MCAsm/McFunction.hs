{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Language.Cobble.MCAsm.McFunction where

import Language.Cobble.Prelude

import qualified Data.Text as T

import qualified GHC.Show as S
import Language.Cobble.Shared

newtype McFunction = McFunction { runMcFunction :: Text } deriving (Show, Eq)

newtype Objective = Objective { renderObjective :: Text } deriving (Show, Eq)

newtype Tag = Tag { renderTag :: Text } deriving (Show, Eq)

rawCommand :: Text -> McFunction
rawCommand = McFunction 

addScoreboardObjective :: Objective -> McFunction
addScoreboardObjective t = McFunction $ "scoreboard objectives add " <> (renderObjective t) <> " dummy"

removeScoreboardObjective :: Objective -> McFunction
removeScoreboardObjective t = McFunction $ "scoreboard objectives remove " <> (renderObjective t)



setScoreboardSidebar :: Objective -> McFunction
setScoreboardSidebar objective = McFunction $ "scoreboard objectives setdisplay sidebar " <> renderObjective objective

scoreboardOperation :: Objective -> Text -> SOperation -> Objective -> Text -> McFunction
scoreboardOperation o1 p1 sop o2 p2 =
    McFunction $ "scoreboard players operation " <>p1 <> " " <> renderObjective o1 <> " " <> show sop
        <> " " <> p2 <> " " <> renderObjective o2

data SOperation = SAdd | SSub | SMul | SDiv | SMod | SAssign | SMin | SMax | SSwap deriving Eq

setScoreboardForPlayer :: Objective -> Text -> Int -> McFunction
setScoreboardForPlayer objective player value =
    McFunction $ "scoreboard players set " <> player <> " " <> renderObjective objective <> " " <> show value

addScoreboardForPlayer :: Objective -> Text -> Int -> McFunction
addScoreboardForPlayer o p i = McFunction $ "scoreboard players add " <> p <> " " <> renderObjective o <> " " <> show i

subScoreboardForPlayer :: Objective -> Text -> Int -> McFunction
subScoreboardForPlayer o p i = McFunction $ "scoreboard players remove " <> p <> " " <> renderObjective o <> " " <> show i

resetScoreboardForPlayer :: Objective -> Text -> McFunction
resetScoreboardForPlayer o p = McFunction $ "scoreboard players reset " <> p <> " " <> renderObjective o

moveScoreboard :: Objective -> Text -> Objective -> Text -> McFunction
moveScoreboard o1 p1 o2 p2 = scoreboardOperation o1 p1 SAssign o2 p2

summonMarkerWithTags :: [Tag] -> McFunction
summonMarkerWithTags = summonMarkerAtWithTags 0 0 0

summonMarkerAtWithTags :: Pos -> Pos -> Pos -> [Tag] -> McFunction
summonMarkerAtWithTags x y z tags = McFunction $ "summon minecraft:area_of_effect_cloud " <> T.unwords [show x, show y, show z]
                                <> " {Duration: 2147483647, Tags:["
                                <> T.intercalate "," (map (show . renderTag) tags)
                                <> "]}"

removeTag :: Tag -> Text -> McFunction
removeTag tag ent = McFunction $ "tag " <> ent <> " remove " <> renderTag tag   

runFunction :: Text -> QualifiedName -> McFunction
runFunction ns f = McFunction $ "function " <> ns <> ":" <> show f

execute :: ExecuteAction -> McFunction
execute = McFunction . mappend "execute " . runExecAction

runExecAction :: ExecuteAction -> Text
runExecAction = \case
    ERun (McFunction f) -> "run " <> f
    EAs       e a     -> "as " <> e <> " " <> runExecAction a
    EAt       e a     -> "at " <> e <> " " <> runExecAction a
    EIn       d a     -> "in " <> show d <> " " <> runExecAction a
    EAnchored d a     -> "anchored " <> show d <> " " <> runExecAction a
    EStoreRes s a     -> "store result " <> show s <> " " <> runExecAction a
    EStoreSuccess s a -> "store success " <> show s <> " " <> runExecAction a
    EFacing efp a     -> "facing " <> show efp <> " " <> runExecAction a
    EIf eip a         -> "if " <> show eip <> " " <> runExecAction a

data ExecuteAction = EAs Text ExecuteAction
                   | EAt Text ExecuteAction
                   | EIn Dimension ExecuteAction
                   | EAnchored Anchor ExecuteAction
                   | EStoreRes Store ExecuteAction
                   | EStoreSuccess Store ExecuteAction
                   | EFacing EFaceParam ExecuteAction
                   | EIf EIfParam ExecuteAction
                   | ERun McFunction
                   -- TODO: EAlign
                   deriving Eq

data EIfParam = EIBlock Pos Pos Pos Text
              | EIBlocks -- TODO
              | EIData --TODO
              | EIEntity Text
              | EIPredicate QualifiedName
              | EIScore Objective Text EIScoreOp
              deriving Eq

instance S.Show EIfParam where
    show = \case
        EIBlock x y z t     -> "block " <> show x <> " " <> show y <> " " <> show z <> " " <> toString t
        EIBlocks            -> error "blocks NYI"
        EIData              -> error "data NYI"
        EIEntity e          -> "entity " <> toString e
        EIPredicate qn      -> "predicate " <> show qn
        EIScore obj pl so   -> toString $ "score " <> pl <> " " <> renderObjective obj <> " " <> show so

data EIScoreOp = EILT Objective Text
               | EIGT Objective Text
               | EILE Objective Text
               | EIGE Objective Text
               | EIEQ Objective Text
               | EIMatches Range
               deriving (Eq)

instance S.Show EIScoreOp where 
    show = \case
        EILT obj pl -> toString $ "< " <> pl <> " " <> renderObjective obj
        EIGT obj pl -> toString $ "> " <> pl <> " " <> renderObjective obj
        EILE obj pl -> toString $ "<= " <> pl <> " " <> renderObjective obj
        EIGE obj pl -> toString $ ">= " <> pl <> " " <> renderObjective obj
        EIEQ obj pl -> toString $ "= " <> pl <> " " <> renderObjective obj
        EIMatches r -> "matches " <> show r 
  
data Range = RInfEnd Int
           | RInfStart Int
           | RBounded Int Int
           deriving (Eq)
           
instance S.Show Range where
    show = \case
        RInfEnd i           -> show i <> ".."
        RInfStart i         ->  ".." <> show i
        RBounded minr maxr  ->  show minr <> ".." <> show maxr

data EFaceParam = EFEntity Text Anchor
                | EFPos Pos Pos Pos
                deriving Eq

instance S.Show EFaceParam where
    show = \case
        EFEntity t a -> "entity " <> toString t <> " " <> show a
        EFPos x y z -> toString $ unwords (map show [x, y, z])

data Pos = Abs Int
         | Rel Int
         deriving (Eq)

mapPos :: (Int -> Int) -> Pos -> Pos
mapPos f (Abs x) = Abs (f x)
mapPos f (Rel x) = Rel (f x)

liftPos2 :: (Int -> Int -> Int) -> Pos -> Pos -> Pos
liftPos2 f (Rel x) (Rel y) = Rel (f x y)
liftPos2 f (Rel x) (Abs y) = Rel (f x y)
liftPos2 f (Abs x) (Abs y) = Abs (f x y)
liftPos2 f (Abs x) (Rel y) = Rel (f x y)

instance S.Show Pos where
    show = \case
        Abs i -> show i
        Rel i -> "~" <> show i

instance Num Pos where
    fromInteger = Abs . fromInteger
    (+)    = liftPos2 (+)
    (*)    = liftPos2 (*)
    (-)    = liftPos2 (-)
    abs    = mapPos abs
    signum = mapPos signum

data Dimension = Overworld | Nether | End deriving Eq

type NBTPath = Text

data Store = StBlock Pos Pos Pos NBTPath Int
           | StScore Objective Text
           | StEntity Text NBTPath Int
           | StBossbar Text BossbarStore
           | StStorage Text NBTPath Int
           deriving Eq

data BossbarStore = BMax | BVal deriving Eq

instance S.Show BossbarStore where
    show BMax = "max"
    show BVal = "value"

instance S.Show Store where
    show = \case
        StBlock x y z nb s -> "block " <> show x <> " " <> show y <> " " <> show z <> " " <> toString nb <> " double " <> show s
        StScore obj player -> toString $ "score " <> player <> " " <> renderObjective obj
        StEntity e nb s    -> "entity " <> toString e <> " " <> toString nb <> " " <> show s
        StBossbar bid bs   -> "bossbar " <> toString bid <> " " <> show bs
        StStorage p n s    -> "storage " <> toString p <> " " <> toString n <> " " <> show s

data Anchor = Eyes | Feet deriving Eq

instance S.Show Anchor where
    show Eyes = "eyes"
    show Feet = "feet"

instance S.Show Dimension where
    show Overworld = "minecraft:overworld"
    show Nether    = "minecraft:the_nether"
    show End       = "minecraft:the_end"

instance S.Show SOperation where
    show = \case
        SAdd    -> "+="
        SSub    -> "-="
        SMul    -> "*="
        SDiv    -> "/="
        SMod    -> "%="
        SAssign -> "="
        SMin    -> "<"
        SMax    -> ">"
        SSwap   -> "><"

