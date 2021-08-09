{-#LANGUAGE NoOverloadedStrings#-}
module Language.Cobble.McFunction.PrettyPrint where

import Language.Cobble.Prelude hiding (List, Tell, Get)

import Relude (absurd)
import qualified Data.Text as T
import Language.Cobble.Shared (QualifiedName)
import Language.Cobble.Codegen.Common
import Language.Cobble.McFunction.Types

class PrettyPrint a where
    prettyPrint :: a -> Text

(<->) :: (PrettyPrint a, PrettyPrint b) => a -> b -> Text
x <-> y = prettyPrint x <> prettyPrint y

(<:>) :: (PrettyPrint a, PrettyPrint b) => a -> b -> Text
x <:> y = prettyPrint x <> toText " " <> prettyPrint y
infixr 5 <:>

(<:?>) :: (PrettyPrint a, PrettyPrint b) => a -> Maybe b -> Text
x <:?> Nothing = prettyPrint x
x <:?> Just y  = x <:> y
infixr 5 <:?>

instance PrettyPrint Command where
    prettyPrint = \case 
        Advancement vo           -> absurd vo
        Attribute vo             -> absurd vo
        Bossbar vo               -> absurd vo
        Clear vo                 -> absurd vo
        Clone vo                 -> absurd vo
        Data vo                  -> absurd vo
        Datapack vo              -> absurd vo
        Debug vo                 -> absurd vo
        DefaultGamemode gamemode -> "defaultgamemode" <:> gamemode
        Difficulty dif           -> "difficulty" <:> dif
        Effect vo                -> absurd vo
        Enchant sel ench m_level -> "enchant" <:> sel <:> ench <:?> m_level
        Execute ea               -> "execute" <:> ea
        Experience vo            -> absurd vo
        Fill vo                  -> absurd vo
        Forceload vo             -> absurd vo
        Function fname           -> "function" <:> fname
        Gamemode gamemode msel   -> "gamemode" <:> gamemode <:?> msel
        Gamerule vo              -> absurd vo
        Give sel item mamount    -> "give" <:> sel <:> item <:?> mamount
        Help                     -> toText "help"
        Item vo                  -> absurd vo
        Kick sel mreason         -> "kick" <:> sel <:?> mreason
        Kill sel                 -> "kill" <:> sel
        List                     -> toText "list"
        ListUUIDs                -> "list" <:> "uuids"
        Locate structure         -> "locate" <:> structure
        LocateBiome biome        -> "locatebiome" <:> biome
        Loot vo                  -> absurd vo
        Me txt                   -> "me" <:> txt
        Msg sel msg              -> "msg" <:> sel <:> msg
        Particle vo              -> absurd vo
        Playsound vo             -> absurd vo
        Publish                  -> toText "publish"
        Recipe vo                -> absurd vo
        Reload                   -> toText "reload"
        Say txt                  -> "say" <:> txt
        Schedule sa qn           -> "schedule" <:> sa <:> qn
        Scoreboard sa            -> "scoreboard" <:> sa
        Seed                     -> toText "seed"
        Setblock vo              -> absurd vo
        Setworldspawn vo         -> absurd vo
        Spawnpoint vo            -> absurd vo
        Spectate vo              -> absurd vo
        Spreadplayers vo         -> absurd vo
        StopSound vo             -> absurd vo
        Summon entity sa         -> "summon" <:> entity <:?> sa
        Tag sel ta               -> "tag" <:> sel <:> ta
        Team vo                  -> absurd vo
        TeamMsg txt              -> "teammsg" <:> txt
        Teleport vo              -> absurd vo
        Tell se txt              -> "tell" <:> se <:> txt
        TellRaw se txt           -> "tellraw" <:> se <:> txt
        Time vo                  -> absurd vo
        Title vo                 -> absurd vo
        TM txt                   -> "tm" <:> txt
        Tp vo                    -> absurd vo
        Trigger vo               -> absurd vo
        Weather we               -> "weather" <:> we
        Worldboarder vo          -> absurd vo
        Xp vo                    -> absurd vo

instance PrettyPrint Gamemode where
    prettyPrint = toText . \case
        Survival  -> "survival"
        Creative  -> "creative"
        Adventure -> "adventure"
        Spectator -> "spectator"

instance PrettyPrint Difficulty  where 
    prettyPrint = toText . \case  
        Peaceful -> "peaceful"
        Easy     -> "easy"
        Normal   -> "normal"
        Hard     -> "hard"

instance PrettyPrint Weather where
    prettyPrint = toText . \case
       ClearW -> "clear"
       Rain -> "rain"
       Thunder -> "thunder"

instance PrettyPrint SummonArg where
    prettyPrint (SummonArg pos mnbt) = pos <:?> mnbt

instance PrettyPrint ScoreboardArg where
    prettyPrint = \case 
        Players pa    -> "players" <:> prettyPrint pa
        Objectives oa -> "objectives" <:> prettyPrint oa

instance PrettyPrint PlayerScoreboardArg where
    prettyPrint = \case
        Add sel obj amount          -> "add" <:> sel <:> obj <:> amount
        Enable sel obj              -> "enable" <:> sel <:> obj
        Get sel obj                 -> "get" <:> sel <:> obj
        SList msel                  -> "list" <:?> msel
        Remove sel obj amount       -> "remove" <:> sel <:> obj <:> amount
        Reset sel mobj              -> "reset" <:> sel <:?> mobj
        Set sel obj amount          -> "set" <:> sel <:> obj <:> amount
        Operation s1 o1 op s2 o2    -> "operation" <:> s1 <:> o1 <:> op <:> s2 <:> o2

instance PrettyPrint SOperation where
    prettyPrint = toText . \case
        SMod    -> "%="
        SMul    -> "*="
        SAdd    -> "+="
        SSub    -> "-="
        SDiv    -> "/="
        SMin    -> "<"
        SAssign -> "="
        SMax    -> ">"
        SSwap   -> "><"

instance PrettyPrint ObjectiveScoreboardArg where
    prettyPrint = \case
        OAdd obj name dname -> "add" <:> obj <:> name <:?> dname
        ORemove obj         -> "remove" <:> obj
        OModify m           -> "modify" <:> m
        OList               -> toText "list"
        OSetDisplay obj dis -> "setdisplay" <:> obj <:> dis

instance PrettyPrint OModifyArg where
    prettyPrint = \case
        DisplayName n -> "displayname" <:> n
        RenderType t  -> "rendertype" <:> t

instance PrettyPrint OModifyRenderType where
    prettyPrint = toText . \case
        RHearts   -> "hearts"
        RIntegers -> "integers"

instance PrettyPrint Position where
    prettyPrint = \case
      Abs x y z -> x <:> y <:> z
      Rel x y z -> ("~" <-> x) <:> ("~" <-> y) <:> ("~" <-> z) 
    
instance PrettyPrint NBT where
    prettyPrint = \case
      C fs      -> "{" <-> T.intercalate (toText ",") (map (\(k, v) -> k <-> "=" <-> v) fs) <-> "}"
      S str     -> str
      I n       -> show n
      L nbts    -> "[" <-> T.intercalate (toText ",") (map prettyPrint nbts) <-> "]"

instance PrettyPrint Selector where
    prettyPrint = \case
        Player p            -> p
        Entity args         -> withArgs "@e" args
        AllPlayers args     -> withArgs "@a" args
        NearestPlayer args  -> withArgs "@p" args
        RandomPlayer args   -> withArgs "@r" args
        Self args           -> withArgs "@s" args
        where
            withArgs s []   = toText s
            withArgs s args = toText (s <> "[") <> T.intercalate (toText ", ") (map prettyPrint args) <> toText "]"

instance PrettyPrint SelectorArg where
    prettyPrint = \case
        SType t         -> "type=" <-> prettyPrint t
        SPredicate p    -> "predicate=" <-> prettyPrint p
        SScores ss      -> "scores={" <-> T.intercalate (toText ",") (map (\(o, s) -> o <-> "=" <-> s) ss) <-> "}"
        STag t          -> "tag=" <-> t

instance PrettyPrint ScheduleArg where
    prettyPrint = toText . \case
        SClear      -> "clear"
        SFunction   -> "function"

instance PrettyPrint ExecuteArg where
    prettyPrint = \case
          EAlign vo ea      -> absurd vo
          EAnchored arg ea  -> "anchored" <:> arg <:> ea
          EAs sel ea        -> "as" <:> sel <:> ea
          EAt sel ea        -> "at" <:> sel <:> ea
          EFacing vo ea     -> absurd vo
          EIf ia ea         -> "if" <:> ia <:> ea
          EIn dim ea        -> "in" <:> dim <:> ea
          EPositioned vo ea -> absurd vo
          ERotated vo ea    -> absurd vo
          EStore vo ea      -> absurd vo
          EUnless vo ea     -> absurd vo
          ERun c            -> "run" <:> c

instance PrettyPrint EAnchoredArg where
    prettyPrint = toText . \case
        Eyes -> "eyes"
        Feet -> "feet"

instance PrettyPrint EIfArg where
    prettyPrint = \case
        IBlock v            -> absurd v
        IBlocks v           -> absurd v
        IData v             -> absurd v
        IEntity sel         -> "entity" <:> sel
        IPredicate p        -> "predicate" <:> p
        IScore sel obj a    -> "score" <:> sel <:> obj <:> a

instance PrettyPrint IfScoreArg where
    prettyPrint = \case
        ILT s o     -> "<"  <:> s <:> o
        ILE s o     -> "<=" <:> s <:> o
        IEQ s o     -> "="  <:> s <:> o
        IGT s o     -> ">"  <:> s <:> o
        IGE s o     -> ">=" <:> s <:> o
        IMatches r  -> "matches" <:> r 

instance PrettyPrint TagArg where
    prettyPrint
      = \case
          TRemove t -> "remove" <:> t
          TAdd t    -> "add" <:> t
          TList     -> toText "list"

instance PrettyPrint Range where
    prettyPrint = \case
        x :.. y -> x <-> ".." <-> y
        RLE x   -> x <-> ".."
        RGE y   ->       ".." <-> y
        REQ x   -> prettyPrint x

instance PrettyPrint NamespacedName where
    prettyPrint = show @_ @QualifiedName -- The type annotation forces a type error once
                                         -- NamespacedName becomes its own type
instance PrettyPrint Int where
    prettyPrint = show

instance PrettyPrint Text where
    prettyPrint = id

instance PrettyPrint String where
    prettyPrint = toText

