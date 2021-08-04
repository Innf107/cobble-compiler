module Language.Cobble.MCAsm.Compiler where

import Language.Cobble.Prelude hiding (ix)

import Language.Cobble.Types (Panic(..))

import Language.Cobble.Shared

import qualified Data.Text as T

import Language.Cobble.MCAsm.Types

import Language.Cobble.MCAsm.McFunction

initialize :: (CompC r) => Sem r IntermediateResult
initialize = InterModule "init" <$> instr do
        rawCommand "gamerule maxCommandChainLength 2147483647"
        rawCommand "forceload add 0 0"
        addScoreboardObjective regs
        addScoreboardObjective aelem
        addScoreboardObjective uid
        addScoreboardObjective ix
        --setScoreboardForPlayer uid "UID" 0
        setScoreboardForPlayer regs "TRUE" 1
        --setScoreboardForPlayer regs "NFALSE" 0
        whenDebug $ setScoreboardSidebar regs


clean :: (CompC r) => Sem r IntermediateResult
clean = InterModule "clean" <$> instr do
    removeScoreboardObjective regs
    removeScoreboardObjective aelem
    removeScoreboardObjective uid
    removeScoreboardObjective ix

hoistModules :: (CompC r) => IntermediateResult -> Sem r [CompiledModule]
hoistModules (InterInstructions _) = throw $ Panic "Cannot create modules from standalone top level instructions"
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
               . concat <$> mapM (uncurry makeModulesInner) mods

compile :: (CompC r) => [Module] -> Sem r [CompiledModule]
compile mods = do
    log LogVerbose "STARTING MCASM Codegen"
    fmap concat $ mapM hoistModules =<< ((++) <$> sequenceA [initialize,clean] <*> mapM compileModule mods)

compileModule :: (CompC r) => Module -> Sem r IntermediateResult
compileModule (Module {moduleName, moduleInstructions}) = do
    log LogDebug $ "COMPILING MODULE " <> show moduleName
    results <- mapM compileInstr moduleInstructions
    pure $ InterModule moduleName (concat results)

compileInstr :: (CompC r) => Instruction -> Sem r [IntermediateResult]
compileInstr i = do
    log LogDebugVerbose $ "COMPILING INSTRUCTION: " <> show i
    asks nameSpace >>= \ns -> (<>) <$> whenDebugMonoid (pure [comment $ show i]) <*> case i of
        MoveReg reg1 reg2 -> instr (moveReg regs reg1 regs reg2)

        MoveNumLit reg lit          -> instr $ setScoreboardForPlayer regs (renderReg reg) lit
        AddLit reg lit              -> instr $ addReg reg lit
        AddReg reg1 reg2            -> instr $ regOperation reg1 SAdd reg2
        SubLit reg lit              -> instr $ subReg reg lit
        SubReg reg1 reg2            -> instr $ regOperation reg1 SSub reg2
        MulLit reg lit              -> instr $ opLit reg SMul lit
        MulReg reg1 reg2            -> instr $ regOperation reg1 SMul reg2
        DivLit reg lit              -> instr $ opLit reg SDiv lit
        DivReg reg1 reg2            -> instr $ regOperation reg1 SDiv reg2
        ModLit reg lit              -> instr $ opLit reg SMod lit
        ModReg reg1 reg2            -> instr $ regOperation reg1 SMod reg2
        Min reg1 reg2               -> instr $ regOperation reg1 SMin reg2
        Max reg1 reg2               -> instr $ regOperation reg1 SMax reg2
        Section name instrs         -> pure . InterModule name . concat <$> (mapM compileInstr instrs)
        Call section                -> instr $ runFunction ns section
        CallEQ reg1 reg2 f          -> instr $ execute (EIf (eiScoreReg reg1 $ eiEqReg reg2) $ ERun (runFunction ns f))
        CallLT reg1 reg2 f          -> instr $ execute (EIf (eiScoreReg reg1 $ eiLtReg reg2) $ ERun (runFunction ns f))
        CallGT reg1 reg2 f          -> instr $ execute (EIf (eiScoreReg reg1 $ eiGtReg reg2) $ ERun (runFunction ns f))
        CallLE reg1 reg2 f          -> instr $ execute (EIf (eiScoreReg reg1 $ eiLeReg reg2) $ ERun (runFunction ns f))
        CallGE reg1 reg2 f          -> instr $ execute (EIf (eiScoreReg reg1 $ eiGeReg reg2) $ ERun (runFunction ns f))
        CallInRange reg range f     -> instr $ execute (EIf (eiScoreReg reg  $ EIMatches range) $ ERun (runFunction ns f))
        ExecEQ reg1 reg2 is         -> instr $ execute (EIf (eiScoreReg reg1 $ eiEqReg reg2) $ ERun (tell is))
        ExecLT reg1 reg2 is         -> instr $ execute (EIf (eiScoreReg reg1 $ eiLtReg reg2) $ ERun (tell is))
        ExecGT reg1 reg2 is         -> instr $ execute (EIf (eiScoreReg reg1 $ eiGtReg reg2) $ ERun (tell is))
        ExecLE reg1 reg2 is         -> instr $ execute (EIf (eiScoreReg reg1 $ eiLeReg reg2) $ ERun (tell is))
        ExecGE reg1 reg2 is         -> instr $ execute (EIf (eiScoreReg reg1 $ eiGeReg reg2) $ ERun (tell is))
        ExecInRange reg range is    -> instr $ execute (EIf (eiScoreReg reg  $ EIMatches range) $ ERun (tell is))


        GetCommandResult reg command -> instr $
              execute (EStoreRes (stScoreReg reg) $ ERun (tell [command]))

        GetBySelector reg selector -> instr do
              incrementUID
              scoreboardOperation regs ("@e[" <> selector <> ",limit=1]") SAssign uid "UID"
              scoreboardOperation regs (renderReg reg) SAssign uid "UID"

        RunCommandAsEntity reg command -> instr do
              execute $ EAs "@e" $ EIf (EIScore regs "@s" $ EIEQ regs (renderReg reg)) $ ERun $ (tell [command])

        GetInArray reg arr aix -> instr do
            asArrayElem arr aix $ scoreboardOperation regs (renderReg reg) SAssign regs "@s"

        SetInArrayOrNew arr aix reg -> instr do
            asArrayElemOrNew arr aix $ scoreboardOperation regs "@s" SAssign regs (renderReg reg)
        SetInArray arr aix reg -> instr do
            asArrayElem arr aix $ scoreboardOperation regs "@s" SAssign regs (renderReg reg)
        SetNewInArray arr aix reg -> instr do
            asNewArrayElem arr aix $ scoreboardOperation regs "@s" SAssign regs (renderReg reg)
        DestroyInArray arr aix -> instr do
            asArrayElem arr aix $ rawCommand "kill @s"

        SetScoreboard obj player reg -> instr do
            scoreboardOperation obj player SAssign regs (renderReg reg)
        RawCommand cmd -> instr $ rawCommand cmd
    where
        opLit :: (CompInnerC r) => Register -> SOperation -> Int -> Sem r ()
        opLit r s l = do
              setReg constReg l
              regOperation r s constReg
        incrementUID :: (CompInnerC r) => Sem r ()
        incrementUID = addScoreboardForPlayer uid "UID" 1
        incrementCompUID :: CompC r => Sem r Int
        incrementCompUID = gets compUID <* modify (\s -> s{compUID = compUID s + 1})

instr :: Sem (Writer [McFunction] ': r) () -> Sem r [IntermediateResult]
instr = fmap (pure . InterInstructions . fst) . runWriterAssocR


asArrayElemOrNew :: (CompInnerC r) => Register -> Register -> Sem r () -> Sem r ()
asArrayElemOrNew areg ixreg mcf = do
    setReg elseReg 1
    asArrayElem areg ixreg $ setReg elseReg 0
    execute $ EIf (eiScoreReg elseReg $ EIMatches (RBounded 1 1)) $ ERun $ summonMarkerWithTags [arrayTag, tempTag]
    moveScoreboard aelem ("@e[tag=" <> renderTag tempTag <> "]") regs (renderReg areg)
    moveScoreboard ix ("@e[tag=" <> renderTag tempTag <> "]") regs (renderReg ixreg)
    removeTag tempTag ("@e[tag=" <> renderTag tempTag <> "]")
    asArrayElem areg ixreg mcf


asArrayElemOrIfNotPresent :: (CompInnerC r) => Register -> Register -> Sem r () -> Sem r () -> Sem r ()
asArrayElemOrIfNotPresent areg ixreg mcf inp = do
      setReg elseReg 0
      asArrayElem areg ixreg $ setReg elseReg 1
      execute $ EIf (eiScoreReg elseReg $ EIMatches (RBounded 1 1)) $ ERun $ asArrayElem areg ixreg mcf
      execute $ EIf (eiScoreReg elseReg $ EIMatches (RBounded 0 0)) $ ERun $ inp

asArrayElem :: (CompInnerC r) => Register -> Register -> Sem r () -> Sem r ()
asArrayElem areg ixreg mcf = execute
                        $ EAs "@e[tag=ARRAY]"
                        $ EIf (EIScore aelem "@s" $ EIEQ regs (renderReg areg))
                        $ EIf (EIScore ix "@s" $ EIEQ regs (renderReg ixreg))
                        $ ERun mcf

asNewArrayElem :: (CompInnerC r) => Register -> Register -> Sem r () -> Sem r ()
asNewArrayElem areg ixreg mcf = do
    summonMarkerWithTags [arrayTag, tempTag]
    moveScoreboard aelem ("@e[tag=" <> renderTag tempTag <> "]") regs (renderReg areg)
    moveScoreboard ix ("@e[tag=" <> renderTag tempTag <> "]") regs (renderReg ixreg)
    removeTag tempTag ("@e[tag=" <> renderTag tempTag <> "]")
    asArrayElem areg ixreg mcf

whenDebugMonoid :: (CompC r, Monoid a) => Sem r a -> Sem r a
whenDebugMonoid s = asks debug >>= bool (pure mempty) s

whenDebugAlt :: (CompC r, Alternative f) => Sem r (f a) -> Sem r (f a)
whenDebugAlt s = asks debug >>= bool (pure empty) s

whenDebug :: (CompC r) => Sem r () -> Sem r ()
whenDebug s = asks debug >>= flip when s

constReg :: Register
constReg = NamedReg "CONST"

elseReg :: Register
elseReg = NamedReg "ELSE"

arrayCounterReg :: Register
arrayCounterReg = NamedReg "ARRAYCOUNTER"

regs, aelem, ix, uid :: Objective
regs  = Objective "REGS"
aelem = Objective "AELEM"
ix    = Objective "IX"
uid   = Objective "UID"

tempTag :: Tag
tempTag = Tag "TEMP"

arrayTag :: Tag
arrayTag = Tag "ARRAY"

setReg :: (CompInnerC r) => Register -> Int -> Sem r ()
setReg r = setScoreboardForPlayer regs $ renderReg r

addReg :: (CompInnerC r) => Register -> Int -> Sem r ()
addReg r = addScoreboardForPlayer regs (renderReg r)

subReg :: (CompInnerC r) => Register -> Int -> Sem r ()
subReg r = subScoreboardForPlayer regs (renderReg r)

eiScoreReg :: Register -> EIScoreOp -> EIfParam
eiScoreReg r = EIScore regs (renderReg r)

eiEqReg, eiLtReg, eiGtReg, eiLeReg, eiGeReg :: Register -> EIScoreOp
eiEqReg r = EIEQ regs (renderReg r)
eiLtReg r = EILT regs (renderReg r)
eiGtReg r = EIGT regs (renderReg r)
eiLeReg r = EILE regs (renderReg r)
eiGeReg r = EIGE regs (renderReg r)

stScoreReg :: Register -> Store
stScoreReg r = StScore regs $ renderReg r

regOperation :: (CompInnerC r) => Register -> SOperation -> Register -> Sem r ()
regOperation r1 o r2 = scoreboardOperation regs (renderReg r1) o regs (renderReg r2)

moveReg :: (CompInnerC r) => Objective -> Register -> Objective -> Register -> Sem r ()
moveReg o1 r1 o2 r2 = moveScoreboard o1 (renderReg r1) o2 (renderReg r2)

comment :: Text -> IntermediateResult
comment = InterInstructions . pure . McFunction . ("#" <>)

