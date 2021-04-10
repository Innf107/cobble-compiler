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
        addScoreboardObjective (objForType (Proxy :: Proxy 'Number))
        addScoreboardObjective (objForType (Proxy :: Proxy 'Entity))
        addScoreboardObjective (objForType (Proxy :: Proxy 'Array))
        addScoreboardObjective uid
        addScoreboardObjective aelem
        addScoreboardObjective ix
        setScoreboardForPlayer uid "UID" 0
        whenDebug $ setScoreboardSidebar (objForType (Proxy :: Proxy 'Number))


clean :: (CompC r) => Sem r IntermediateResult
clean = InterModule "clean" <$> instr do
    removeScoreboardObjective (objForType (Proxy :: Proxy 'Number))
    removeScoreboardObjective (objForType (Proxy :: Proxy 'Entity))
    removeScoreboardObjective (objForType (Proxy :: Proxy 'Array))
    removeScoreboardObjective uid
    removeScoreboardObjective aelem
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
               . concat <$> mapM (\(n, inner) -> makeModulesInner n inner) mods

compile :: (CompC r) => [Module] -> Sem r [CompiledModule]
compile mods = do
    log LogVerbose "STARTING MCASM Codegen"
    fmap concat $ join $ mapM hoistModules <$> ((++) <$> sequenceA [initialize,clean] <*> mapM compileModule mods)

compileModule :: (CompC r) => Module -> Sem r IntermediateResult
compileModule (Module {moduleName, moduleInstructions}) = do
    log LogDebug $ "COMPILING MODULE " <> show moduleName
    results <- mapM compileInstr $ moduleInstructions
    pure $ InterModule moduleName (concat results)

compileInstr :: (CompC r) => Instruction -> Sem r [IntermediateResult]
compileInstr i = do
    log LogDebugVerbose $ "COMPILING INSTRUCTION: " <> show i
    asks nameSpace >>= \ns -> (<>) <$> whenDebugMonoid (pure [comment $ show i]) <*> case i of
        MoveReg reg1 reg2 -> instr $ moveReg (objForType reg1) reg1 (objForType reg2) reg2

        MoveNumLit reg lit          -> instr $ setScoreboardForPlayer (objForType reg) (renderReg reg) lit
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
              scoreboardOperation (objForType reg) ("@e[" <> selector <> ",limit=1]") SAssign uid "UID"
              scoreboardOperation (objForType reg) (renderReg reg) SAssign uid "UID"

        RunCommandAsEntity reg command -> instr do
              execute $ EAs "@e" $ EIf (EIScore (objForType reg) "@s" $ EIEQ (objForType reg) (renderReg reg)) $ ERun $ (tell [command])

        GetInArray reg arr aix -> instr do
              asArrayElem arr aix $ scoreboardOperation (objForType reg) (renderReg reg) SAssign (objForType reg) "@s"

        SetInArray arr aix reg -> instr $
              asArrayElemOrNew arr aix $ scoreboardOperation (objForType reg) "@s" SAssign (objForType reg) (renderReg reg)

        SetScoreboard obj player reg -> instr do
              scoreboardOperation obj player SAssign (objForType reg) (renderReg reg)
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
      moveScoreboard (objForType areg) ("@e[tag=" <> renderTag tempTag <> "]") (objForType areg) (renderReg areg)
      moveScoreboard ix ("@e[tag=" <> renderTag tempTag <> "]") (objForType ixreg) (renderReg ixreg)
      removeTag tempTag ("@e[tag=" <> renderTag tempTag <> "]") 
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
                        $ EIf (EIScore aelem "@s" $ EIEQ (objForType areg) (renderReg areg))
                        $ EIf (EIScore ix "@s" $ EIEQ (objForType ixreg) (renderReg ixreg))
                        $ ERun mcf

whenDebugMonoid :: (CompC r, Monoid a) => Sem r a -> Sem r a
whenDebugMonoid s = asks debug >>= bool (pure mempty) s

whenDebugAlt :: (CompC r, Alternative f) => Sem r (f a) -> Sem r (f a)
whenDebugAlt s = asks debug >>= bool (pure empty) s

whenDebug :: (CompC r) => Sem r () -> Sem r ()
whenDebug s = asks debug >>= flip when s

constReg :: Register 'Number
constReg = NumReg (NamedReg "CONST")

elseReg :: Register 'Number
elseReg = NumReg (NamedReg "ELSE")

arrayCounterReg :: Register 'Number
arrayCounterReg = NumReg (NamedReg "ARRAYCOUNTER")

aelem, ix, uid :: Objective
aelem = Objective "AELEM"
ix    = Objective "IX"
uid   = Objective "UID"

tempTag :: Tag
tempTag = Tag "TEMP"

arrayTag :: Tag
arrayTag = Tag "ARRAY"

setReg :: (CompInnerC r) => Register 'Number -> Int -> Sem r ()
setReg r = setScoreboardForPlayer (objForType r) $ renderReg r

addReg :: (CompInnerC r) => Register 'Number -> Int -> Sem r ()
addReg r = addScoreboardForPlayer (objForType r) (renderReg r)

subReg :: (CompInnerC r) => Register 'Number -> Int -> Sem r ()
subReg r = subScoreboardForPlayer (objForType r) (renderReg r)

eiScoreReg :: Register 'Number -> EIScoreOp -> EIfParam
eiScoreReg r = EIScore (objForType r) (renderReg r)

eiEqReg, eiLtReg, eiGtReg, eiLeReg, eiGeReg :: Register 'Number -> EIScoreOp
eiEqReg r = EIEQ (objForType r) (renderReg r)
eiLtReg r = EILT (objForType r) (renderReg r)
eiGtReg r = EIGT (objForType r) (renderReg r)
eiLeReg r = EILE (objForType r) (renderReg r)
eiGeReg r = EIGE (objForType r) (renderReg r)

stScoreReg :: Register 'Number -> Store
stScoreReg r = StScore (objForType r) $ renderReg r

regOperation :: (CompInnerC r) => Register 'Number -> SOperation -> Register 'Number -> Sem r ()
regOperation r1 o r2 = scoreboardOperation (objForType r1) (renderReg r1) o (objForType r2) (renderReg r2)

moveReg :: (CompInnerC r) => Objective -> Register a -> Objective -> Register b -> Sem r ()
moveReg o1 r1 o2 r2 = moveScoreboard o1 (renderReg r1) o2 (renderReg r2)

comment :: Text -> IntermediateResult
comment = InterInstructions . pure . McFunction . ("#" <>)
