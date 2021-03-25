module Language.Cobble.MCAsm.McFunction where

import Language.Cobble.Prelude

import Language.Cobble.MCAsm.Types

import qualified Data.Text as T

import qualified GHC.Show as S
import Language.Cobble.Shared

rawCommand :: (CompInnerC r) => Text -> Sem r ()
rawCommand = tell . pure . McFunction

addScoreboardObjective :: (CompInnerC r) => Objective -> Sem r ()
addScoreboardObjective t = rawCommand $ "scoreboard objectives add " <> (renderObjective t) <> " dummy"

removeScoreboardObjective :: (CompInnerC r) => Objective -> Sem r ()
removeScoreboardObjective t = rawCommand $ "scoreboard objectives remove " <> (renderObjective t)



setScoreboardSidebar :: (CompInnerC r) => Objective -> Sem r ()
setScoreboardSidebar objective = rawCommand $ "scoreboard objectives setdisplay sidebar " <> renderObjective objective

scoreboardOperation :: (CompInnerC r) => Objective -> Text -> SOperation -> Objective -> Text -> Sem r ()
scoreboardOperation o1 p1 sop o2 p2 =
    rawCommand $ "scoreboard players operation " <> p1 <> " " <> renderObjective o1 <> " " <> show sop
        <> " " <> p2 <> " " <> renderObjective o2

data SOperation = SAdd | SSub | SMul | SDiv | SMod | SAssign | SMin | SMax | SSwap deriving Eq

setScoreboardForPlayer :: (CompInnerC r) => Objective -> Text -> Int -> Sem r ()
setScoreboardForPlayer objective player value =
    rawCommand $ "scoreboard players set " <> player <> " " <> renderObjective objective <> " " <> show value

addScoreboardForPlayer :: (CompInnerC r) => Objective -> Text -> Int -> Sem r ()
addScoreboardForPlayer o p i = rawCommand $ "scoreboard players add " <> p <> " " <> renderObjective o <> " " <> show i

subScoreboardForPlayer :: (CompInnerC r) => Objective -> Text -> Int -> Sem r ()
subScoreboardForPlayer o p i = rawCommand $ "scoreboard players remove " <> p <> " " <> renderObjective o <> " " <> show i

resetScoreboardForPlayer :: (CompInnerC r) => Objective -> Text -> Sem r ()
resetScoreboardForPlayer o p = rawCommand $ "scoreboard players reset " <> p <> " " <> renderObjective o

moveScoreboard :: (CompInnerC r) => Objective -> Text -> Objective -> Text -> Sem r ()
moveScoreboard o1 p1 o2 p2 = scoreboardOperation o1 p1 SAssign o2 p2

summonMarkerWithTags :: (CompInnerC r) => [Tag] -> Sem r ()
summonMarkerWithTags = summonMarkerAtWithTags 0 0 0

summonMarkerAtWithTags :: (CompInnerC r) => Pos -> Pos -> Pos -> [Tag] -> Sem r ()
summonMarkerAtWithTags x y z tags = rawCommand $ "summon minecraft:area_effect_cloud " <> T.unwords [show x, show y, show z]
                                <> " {Duration: 2147483647, Tags:["
                                <> T.intercalate "," (map renderTag tags)
                                <> "]}"

removeTag :: (CompInnerC r) => Tag -> Text -> Sem r ()
removeTag tag ent = rawCommand $ "tag " <> ent <> " remove " <> renderTag tag

runFunction :: (CompInnerC r) => Text -> QualifiedName -> Sem r ()
runFunction ns f = rawCommand $ "function " <> ns <> ":" <> show f

execute :: (CompInnerC r) => ExecuteAction r -> Sem r ()
execute = censor (map (\f -> McFunction ("execute " <> runMcFunction f))) . runExecAction

runExecAction :: (CompInnerC r) => ExecuteAction r -> Sem r ()
runExecAction = \case
    ERun      f       -> censorMcFunction ("run "<>)                                     f
    EAs       e a     -> censorMcFunction (\x -> "as " <> e <> " " <> x)                 $ runExecAction a
    EAt       e a     -> censorMcFunction (\x -> "at " <> e <> " " <> x)                 $ runExecAction a
    EIn       d a     -> censorMcFunction (\x -> "in " <> show d <> " " <> x)            $ runExecAction a
    EAnchored d a     -> censorMcFunction (\x -> "anchored " <> show d <> " " <> x)      $ runExecAction a
    EStoreRes s a     -> censorMcFunction (\x -> "store result " <> show s <> " " <> x)  $ runExecAction a
    EStoreSuccess s a -> censorMcFunction (\x -> "store success " <> show s <> " " <> x) $ runExecAction a
    EFacing efp a     -> censorMcFunction (\x -> "facing " <> show efp <> " " <> x)      $ runExecAction a
    EIf eip a         -> censorMcFunction (\x -> "if " <> show eip <> " " <> x)          $ runExecAction a

censorMcFunction :: (CompInnerC r) => (Text -> Text) -> Sem r a -> Sem r a
censorMcFunction f = censor (map (McFunction . f . runMcFunction))

data ExecuteAction r = EAs Text (ExecuteAction r)
                   | EAt Text (ExecuteAction r)
                   | EIn Dimension (ExecuteAction r)
                   | EAnchored Anchor (ExecuteAction r)
                   | EStoreRes Store (ExecuteAction r)
                   | EStoreSuccess Store (ExecuteAction r)
                   | EFacing EFaceParam (ExecuteAction r)
                   | EIf EIfParam (ExecuteAction r)
                   | ERun (Sem r ())
                   -- TODO: EAlign

data EIfParam = EIBlock Pos Pos Pos Text
              | EIBlocks -- TODO
              | EIData --TODO
              | EIEntity Text
              | EIPredicate QualifiedName
              | EIScore Objective Text EIScoreOp

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

