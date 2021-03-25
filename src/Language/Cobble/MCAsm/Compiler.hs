module Language.Cobble.MCAsm.Compiler where

import Language.Cobble.Prelude hiding (ix)

import Language.Cobble.Shared

import qualified Data.Text as T

import Language.Cobble.MCAsm.Types

import Language.Cobble.MCAsm.McFunction

initialize :: (CompC r) => Sem r IntermediateResult
initialize = InterModule "init" <$> instr do
        rawCommand "gamerule maxCommandChainLength 2147483647"
        addScoreboardObjective regs
        addScoreboardObjective eptr
        addScoreboardObjective uid
        addScoreboardObjective aptr
        addScoreboardObjective aelem
        addScoreboardObjective ix
        setScoreboardForPlayer uid "UID" 0
        setScoreboardForPlayer regs "0" 0
        setScoreboardForPlayer regs "1" 1
        setScoreboardForPlayer regs "-1" -1
        whenDebug $ setScoreboardSidebar regs


clean :: (CompC r) => Sem r IntermediateResult
clean = InterModule "init" <$> instr do
    removeScoreboardObjective regs
    removeScoreboardObjective eptr
    removeScoreboardObjective uid
    removeScoreboardObjective aptr
    removeScoreboardObjective ix

hoistModules :: (CompC r) => IntermediateResult -> Sem r [CompiledModule]
hoistModules (InterInstructions _) = error "Cannot create modules from standalone top level instructions"
hoistModules (InterModule mn ins) = makeModulesInner mn ins
    where
        makeModulesInner :: Name -> [IntermediateResult] -> Sem r [CompiledModule]
        makeModulesInner mname subInstrs =
            let (mods, instrs) = partitionEithers $
                    map (\case
                        InterModule n rs -> Left (n, rs)
                        InterInstructions is -> Right is
                    ) subInstrs
            in (CompiledModule mname (unlines (map runMcFunction (concat instrs))):)
--                                                                ^ TODO: Is this concat okay?
               . concat <$> mapM (\(n, inner) -> makeModulesInner (mname <> n) inner) mods
--                                                                           ^ TODO: shouldn't submodules be handled by @QualifiedName@s?
compile :: (CompC r) => [Module] -> Sem r [CompiledModule]
compile mods = fmap concat $ join $ mapM hoistModules <$>
    ((++) <$> sequenceA [initialize,clean] <*> mapM compileModule mods)

compileModule :: (CompC r) => Module -> Sem r IntermediateResult
compileModule (Module {moduleName, moduleInstructions}) = do
    results <- mapM compileInstr $ moduleInstructions
    pure $ InterModule moduleName (concat results)

compileInstr :: (CompC r) => Instruction -> Sem r [IntermediateResult]
compileInstr i = asks nameSpace >>= \ns -> (<>) <$> whenDebugMonoid (pure [comment $ show i]) <*> case i of
    MoveNumLit reg lit          -> instr $ setScoreboardForPlayer regs (renderReg reg) lit
    MoveNumReg reg1 reg2        -> instr $ moveReg regs reg1 regs reg2
    AddLit reg lit              -> instr $ addReg reg lit
    AddReg reg1 reg2            -> instr $ regOperation reg1 SAdd reg2
    SubLit reg lit              -> instr $ subReg reg lit
    SubReg reg1 reg2            -> instr $ regOperation reg1 SSub reg2
    MulLit reg lit              -> instr $ opLit reg SMul lit
    MulReg reg1 reg2            -> instr $ regOperation reg1 SMul reg2
    DivLit reg lit              -> instr $ opLit reg SDiv lit
    DivReg reg1 reg2            -> instr $ regOperation reg1 SDiv reg2
    Min reg1 reg2               -> instr $ regOperation reg1 SMin reg2
    Max reg1 reg2               -> instr $ regOperation reg1 SMax reg2
    Section name instrs         -> pure . InterModule name . concat <$> (mapM compileInstr instrs)
    Call section                -> instr $ runFunction ns section
    ExecEQ reg1 reg2 is         -> instr $ execute $ EIf (eiScoreReg reg1 $ eiEqReg reg2) $ ERun (tell is)
    ExecLT reg1 reg2 is         -> instr $ execute $ EIf (eiScoreReg reg1 $ eiLtReg reg2) $ ERun (tell is)
    ExecGT reg1 reg2 is         -> instr $ execute $ EIf (eiScoreReg reg1 $ eiGtReg reg2) $ ERun (tell is)
    ExecLE reg1 reg2 is         -> instr $ execute $ EIf (eiScoreReg reg1 $ eiLeReg reg2) $ ERun (tell is)
    ExecGE reg1 reg2 is         -> instr $ execute $ EIf (eiScoreReg reg1 $ eiGeReg reg2) $ ERun (tell is)
    ExecInRange reg  range is   -> instr do
          execute $ EIf (eiScoreReg reg  $ EIMatches range) $ ERun $ (tell is)


    GetCommandResult reg command -> instr $
          execute $ EStoreRes (stScoreReg reg) $ ERun $ (tell [command])

    GetBySelector reg selector -> instr do
          incrementUID
          scoreboardOperation eptr ("@e[" <> selector <> ",limit=1]") SAssign uid "UID"
          scoreboardOperation eptr (renderReg reg) SAssign uid "UID"

    RunCommandAsEntity reg command -> instr do
          execute $ EAs "@e" $ EIf (EIScore eptr "@s" $ EIEQ eptr (renderReg reg)) $ ERun $ (tell [command])

    MoveEntity reg1 reg2 -> instr $ moveReg eptr reg1 eptr reg2

    GetNumInArray reg arr aix -> instr do
          asArrayElem arr aix $ scoreboardOperation regs (renderReg reg) SAssign regs "@s"


    GetEntityInArray reg arr aix -> instr do
          asArrayElem arr aix $ scoreboardOperation eptr (renderReg reg) SAssign eptr "@s"

    SetNumInArray arr aix reg -> instr $
          asArrayElemOrNew arr aix $ scoreboardOperation regs "@s" SAssign regs (renderReg reg)

    SetEntityInArray arr aix reg -> instr $
          asArrayElemOrNew arr aix $ scoreboardOperation eptr "@s" SAssign eptr (renderReg reg)

    SetScoreboard obj player reg -> instr do
          scoreboardOperation obj player SAssign regs (renderReg reg)

    MoveArray r1 r2 -> instr do
          moveReg aptr r1 aptr r2

    GetArrayInArray reg arr aix -> instr do
          asArrayElem arr aix $ moveScoreboard aptr (renderReg reg) aptr "@s"

    SetArrayInArray arr aix reg -> instr do
          asArrayElemOrNew arr aix $ moveScoreboard aptr "@s" aptr (renderReg reg)
    where
        opLit :: (CompInnerC r) => Register 'Number -> SOperation -> Int -> Sem r ()
        opLit r s l = do
              setReg constReg l
              regOperation r s constReg
        incrementUID :: (CompInnerC r) => Sem r ()
        incrementUID = addScoreboardForPlayer uid "UID" 1
        incrementCompUID :: CompC r => Sem r Int
        incrementCompUID = gets compUID <* modify (\s -> s{compUID = compUID s + 1})

instr :: Sem (Writer [McFunction] ': r) () -> Sem r [IntermediateResult]
instr = fmap (pure . InterInstructions . fst) . runWriterAssocR


asArrayElemOrNew :: (CompInnerC r) => Register 'Array -> Register 'Number -> Sem r () -> Sem r ()
asArrayElemOrNew areg ixreg mcf = asArrayElemOrIfNotPresent areg ixreg mcf do
      summonMarkerWithTags [arrayTag, tempTag]
      moveScoreboard aptr ("@e[tag=" <> renderTag tempTag <> "]") aptr (renderReg areg)
      moveScoreboard ix ("@e[tag=" <> renderTag tempTag <> "]") regs (renderReg ixreg)
      mcf


asArrayElemOrIfNotPresent :: (CompInnerC r) => Register 'Array -> Register 'Number -> Sem r () -> Sem r () -> Sem r ()
asArrayElemOrIfNotPresent areg ixreg mcf inp = do
      setReg elseReg 0
      asArrayElem areg ixreg $ setReg elseReg 1
      execute $ EIf (eiScoreReg elseReg $ EIMatches (RBounded 1 1)) $ ERun $ asArrayElem areg ixreg mcf
      execute $ EIf (eiScoreReg elseReg $ EIMatches (RBounded 0 0)) $ ERun $ inp

asArrayElem :: (CompInnerC r) => Register 'Array -> Register 'Number -> Sem r () -> Sem r ()
asArrayElem areg ixreg mcf = execute
                        $ EAs "@e[tag=ARRAY]"
                        $ EIf (EIScore aelem "@s" $ EIEQ aptr (renderReg areg))
                        $ EIf (EIScore ix "@s" $ EIEQ regs (renderReg ixreg))
                        $ ERun mcf

whenDebugMonoid :: (CompC r, Monoid a) => Sem r a -> Sem r a
whenDebugMonoid s = asks debug >>= bool (pure mempty) s

whenDebugAlt :: (CompC r, Alternative f) => Sem r (f a) -> Sem r (f a)
whenDebugAlt s = asks debug >>= bool (pure empty) s

whenDebug :: (CompC r) => Sem r () -> Sem r ()
whenDebug s = asks debug >>= flip when s

constReg :: Register 'Number
constReg = CustomReg "CONST"

elseReg :: Register 'Number
elseReg = CustomReg "ELSE"

arrayCounterReg :: Register 'Number
arrayCounterReg = CustomReg "ARRAYCOUNTER"

regs, aptr, aelem, ix, eptr, uid :: Objective
regs  = Objective "REGS"
aptr  = Objective "APTR"
aelem = Objective "AELEM"
ix    = Objective "IX"
eptr  = Objective "EPTR"
uid   = Objective "UID"

tempTag :: Tag
tempTag = Tag "TEMP"

arrayTag :: Tag
arrayTag = Tag "ARRAY"

setReg :: (CompInnerC r) => Register 'Number -> Int -> Sem r ()
setReg = setScoreboardForPlayer regs . renderReg

addReg :: (CompInnerC r) => Register 'Number -> Int -> Sem r ()
addReg r = addScoreboardForPlayer regs (renderReg r)

subReg :: (CompInnerC r) => Register 'Number -> Int -> Sem r ()
subReg r = subScoreboardForPlayer regs (renderReg r)

eiScoreReg :: Register 'Number -> EIScoreOp -> EIfParam
eiScoreReg r = EIScore regs (renderReg r)

eiEqReg, eiLtReg, eiGtReg, eiLeReg, eiGeReg :: Register 'Number -> EIScoreOp
eiEqReg r = EIEQ regs (renderReg r)
eiLtReg r = EILT regs (renderReg r)
eiGtReg r = EIGT regs (renderReg r)
eiLeReg r = EILE regs (renderReg r)
eiGeReg r = EIGE regs (renderReg r)

stScoreReg :: Register 'Number -> Store
stScoreReg = StScore regs . renderReg

regOperation :: (CompInnerC r) => Register 'Number -> SOperation -> Register 'Number -> Sem r ()
regOperation r1 o r2 = scoreboardOperation regs (renderReg r1) o regs (renderReg r2)

moveReg :: (CompInnerC r) => Objective -> Register a -> Objective -> Register b -> Sem r ()
moveReg o1 r1 o2 r2 = moveScoreboard o1 (renderReg r1) o2 (renderReg r2)

comment :: Text -> IntermediateResult
comment = InterInstructions . pure . McFunction . ("#" <>)
