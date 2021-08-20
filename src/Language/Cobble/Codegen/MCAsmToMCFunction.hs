module Language.Cobble.Codegen.MCAsmToMCFunction where

import Language.Cobble.Prelude hiding (to, from, para)
import Language.Cobble.Shared
import Language.Cobble.MCAsm.Types as A
import Language.Cobble.McFunction.Types as F
import Language.Cobble.Codegen.Common

import Data.Functor.Foldable hiding (Nil)

compile :: [Block] -> [CompiledModule]
compile blocks  =   mallocMod 
                :   icallMods 
                <>  map (\(Block n is) -> (qNameToPath n, concat $ run $ runReader icallMap $ (traverse compileInstruction is))) blocks 
    where
        (icallMods, icallMap) = createICallTree icalledFunctions
        icalledFunctions = map (\(Block f _) -> f) blocks

regs :: Objective
regs = "REGS"

mallocMod :: CompiledModule 
mallocMod = ("__malloc__", [
        Scoreboard (Players (Operation self aptrObj SAssign (reg aptrReg) aptrObj))
    ,   Scoreboard (Players (Operation self ixObj SAssign (reg ixReg) ixObj))
    ,   Scoreboard (Players (F.Add (reg ixReg) ixObj 1))
    ,   Tag self (TRemove "MALLOC")
    ])

compileInstruction :: forall r. (Members '[Reader (Map QualifiedName Int)] r) 
                   => Instruction 
                   -> Sem r [Command]
compileInstruction = \case
    Move to from                  -> pure [Scoreboard (Players (Operation (reg to) regs SAssign (reg from) regs))]
    MoveLit to n                  -> pure [Scoreboard (Players (Set (reg to) regs n))] 
    A.Add x y                     -> pure [Scoreboard (Players (Operation (reg x) regs SAdd (reg y) regs))] 
    AddLit x n                    -> pure [Scoreboard (Players (F.Add (reg x) regs n))] 
    Sub x y                       -> pure [Scoreboard (Players (Operation (reg x) regs SSub (reg y) regs))]
    SubLit x n                    -> pure [Scoreboard (Players (Remove (reg x) regs n))] 
    Mul x y                       -> pure [Scoreboard (Players (Operation (reg x) regs SMul (reg y) regs))] 
    Div x y                       -> pure [Scoreboard (Players (Operation (reg x) regs SDiv (reg y) regs))]  
    Mod x y                       -> pure [Scoreboard (Players (Operation (reg x) regs SMod (reg y) regs))]  
    Min x y                       -> pure [Scoreboard (Players (Operation (reg x) regs SMin (reg y) regs))]  
    Max x y                       -> pure [Scoreboard (Players (Operation (reg x) regs SMin (reg y) regs))]  
    Call f                        -> pure [Function (own f)]  
    ICall x                       -> pure [
                                        Scoreboard $ Players (Operation (reg icallReg) regs SAssign (reg x) regs)
                                    ,   Function $ ownPath "icall.icall"
                                    ]   
    LoadFunctionAddress x f       -> asks (lookup f) >>= \case 
        Just address -> pure [Scoreboard (Players (Set (reg x) regs address))]
        Nothing -> error $ "Cannot find function address for function '" <> show f <> "'"
    ExecInRange x r i             -> asExec i (EIf (IScore (reg x) regs (IMatches r))      )
    ExecEQ x y i                  -> asExec i (EIf (IScore (reg x) regs (IEQ (reg y) regs))) 
    ExecLT x y i                  -> asExec i (EIf (IScore (reg x) regs (ILT (reg y) regs))) 
    ExecGT x y i                  -> asExec i (EIf (IScore (reg x) regs (IGT (reg y) regs))) 
    ExecLE x y i                  -> asExec i (EIf (IScore (reg x) regs (ILE (reg y) regs))) 
    ExecGE x y i                  -> asExec i (EIf (IScore (reg x) regs (IGE (reg y) regs))) 
    Malloc x n                    -> pure $ concat [
            [Scoreboard (Players (Operation (reg x) regs SAssign (reg aptrReg) aptrObj))]
        ,   replicate n (Summon (Foreign "minecraft" "marker") (Just (SummonArg (Abs 0 0 0) (Just (C [("Tags", L [S "MALLOC"])])))))
        ,   [   Scoreboard (Players (Set (reg ixReg) ixObj 0))
            ,   Execute (EAs (Entity [STag "MALLOC"]) (ERun (Function $ ownPath "__malloc__")))
            ,   Scoreboard (Players (F.Add (reg aptrReg) aptrObj 1))
            ]
        ]  
    Select x a i                  -> pure [
        Execute 
            $ EAs (Entity [SScores [(ixObj, i)]]) 
            $ EIf (IScore self aptrObj (IEQ (reg a) regs)) 
            $ ERun (Scoreboard (Players (Operation (reg x) regs SAssign self regs)))] 
    Store a x i                   -> pure [
        Execute 
            $ EAs (Entity [SScores [(ixObj, i)]]) 
            $ EIf (IScore self aptrObj (IEQ (reg a) regs)) 
            $ ERun (Scoreboard (Players (Operation self regs SAssign (reg x) regs)))] 

    SetScoreboard player obj x  -> pure [Scoreboard (Players (Operation (Player player) obj SAssign (reg x) regs))]
    where 
        asExec :: Instruction -> (ExecuteArg -> ExecuteArg) -> Sem r [Command]
        asExec i e = map (Execute . e . ERun) <$> compileInstruction i 

own :: QualifiedName -> NamespacedName 
own = Own . show

ownPath :: QualifiedName -> NamespacedName 
ownPath = Own . toText . qNameToPath

reg :: Register -> Selector
reg (Reg r)        = Player ("$" <> show r) 
reg (SpecialReg r) = Player ("%" <> r)

self :: Selector
self = Self []

ixReg :: Register
ixReg = SpecialReg "IX"

ixObj :: Objective 
ixObj = "IX"

aptrReg :: Register
aptrReg = SpecialReg "APTR"


aptrObj :: Objective 
aptrObj = "APTR"


icallReg :: Register
icallReg = SpecialReg "ICALL"

icallDoneReg :: Register
icallDoneReg = SpecialReg "ICALLDONE"

data BinTree a = Nil | Node a (BinTree a) (BinTree a) deriving (Show, Eq, Functor, Foldable)

data BinTreeF a t = NilF | NodeF a t t deriving (Show, Eq, Functor, Foldable)

type instance Base (BinTree a) = BinTreeF a
instance Recursive (BinTree a) where
    project Nil = NilF
    project (Node x l r) = NodeF x l r
instance Corecursive (BinTree a) where
    embed NilF = Nil
    embed (NodeF x l r) = Node x l r

nodeValue :: BinTree a -> Maybe a
nodeValue Nil = Nothing
nodeValue (Node x _ _) = Just x

createICallTree :: [QualifiedName] -> ([CompiledModule], Map QualifiedName Int)
createICallTree fs = (,mapping) $ (icallMod:) $ binTree & para \case
        NilF -> []
        NodeF x (lt, lmods) (rt, rmods) -> [("icall/node" <> show x, catMaybes [
                Just $ Execute
                        $ EIf (IScore (reg icallReg) regs $ IMatches (REQ x)) 
                        $ ERun $ Scoreboard (Players (Set (reg icallDoneReg) regs 1))
            ,   Just $ Execute 
                        $ EIf (IScore (reg icallReg) regs $ IMatches (REQ x)) 
                        $ ERun $ Function (getFunction x)
            ,   Execute . EIf (IScore (reg icallDoneReg) regs $ IMatches (REQ 0))
                        . EIf (IScore (reg icallReg) regs $ IMatches (RLE x))
                        . ERun . Function . ownPath . ("icall.node" +.) . show <$> nodeValue lt
            ,   Execute . EIf (IScore (reg icallDoneReg) regs $ IMatches (REQ 0))
                        . EIf (IScore (reg icallReg) regs $ IMatches (RGE x))
                        . ERun . Function . ownPath . ("icall.node" +.) . show <$> nodeValue rt
            ])] ++ lmods ++ rmods
    where
        mapping :: Map QualifiedName Int
        mapping = fromList (zip fs [1..])
        mappingBack :: Map Int QualifiedName
        mappingBack = fromList (zip [1..] fs)
        getFunction :: Int -> NamespacedName
        getFunction i = ownPath $ fromMaybe (error "createICallTree: No function with ID: " <> show i) (lookup i mappingBack)

        binTree = mkBinTree 0 (length fs + 1)

        -- | @left@ and @right@ are *exclusive* bounds
        mkBinTree left right 
            | left == mid = Nil
            | otherwise = Node mid (mkBinTree left mid) (mkBinTree mid right)
            where
                mid = (left + right) `div` 2

        icallMod :: CompiledModule
        icallMod = ("icall/icall", case binTree of
            Nil -> []
            Node start _ _ -> [
                    Scoreboard $ Players $ Set (reg icallDoneReg) regs 0
                ,   Function (ownPath $ "icall.node" +. show start)
                ])


{-
-- | builds a search tree from the supplied list of functions.
-- returns the compiled modules required for the tree, as well as mappings from functions to their addresses.
createICallTree :: [QualifiedName] -> ([CompiledModule], Map QualifiedName Int)
createICallTree fs = over _1 ((icallMod:) . fst) $ swap $ run $ runState mempty (go fs 1 (length fs))
    where

        icallMod :: CompiledModule
        icallMod = ("icall/icall", [
                Scoreboard $ Players $ Set (reg icallDoneReg) regs 0
            ,   Function (ownPath $ "icall.nodes" <> show (length fs `div` 2))
            ])

        addMapping :: (Members '[State (Map QualifiedName Int)] r)
                   => Int 
                   -> QualifiedName  
                   -> Sem r ()
        addMapping i f = modify (insert f i)

        go :: (Members '[State (Map QualifiedName Int)] r) 
           => [QualifiedName] 
           -> Int 
           -> Int 
           -> Sem r ([CompiledModule], [QualifiedName])
        go (f:fs) left right | (left + right) `div` 2 /= right = do
            let mid = (left + right) `div` 2
            addMapping mid f
            let mod = (("icall/nodes/" <> show mid), catMaybes [
                        Just $ Execute
                                $ EIf (IScore (reg icallReg) regs $ IMatches (REQ mid)) 
                                $ ERun $ Scoreboard (Players (Set (reg icallDoneReg) regs 1)) 
                    ,   Just $ Execute 
                                $ EIf (IScore (reg icallReg) regs $ IMatches (REQ mid)) 
                                $ ERun $ Function (ownPath $ ("icall/" <> f))
                    ,   whenAlt ((left + mid) `div` 2 /= mid) $ Execute 
                                $ EIf (IScore (reg icallDoneReg) regs $ IMatches (REQ 0)) 
                                $ EIf (IScore (reg icallReg) regs $ IMatches (RLE mid)) 
                                $ ERun $ Function (ownPath $ "icall.nodes" <> show ((left + mid) `div` 2))
                    ,   whenAlt ((mid + right) `div` 2 /= mid) $ Execute 
                                $ EIf (IScore (reg icallDoneReg) regs $ IMatches (REQ 0)) 
                                $ EIf (IScore (reg icallReg) regs $ IMatches (RGE mid)) 
                                $ ERun $ Function (ownPath $ "icall.nodes" <> show ((mid + right) `div` 2))
                    ])
            (leftMods, fs')   <- go fs  left mid
            (rightMods, fs'') <- go fs' mid right
            pure (mod : leftMods <> rightMods, fs'')
        go fs _ _ = pure ([], fs)
-}

