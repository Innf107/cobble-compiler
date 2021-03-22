{-# LANGUAGE NoImplicitPrelude, LambdaCase, DataKinds, ConstraintKinds, OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}
module Language.Cobble.MCAsm.Compiler where

import Language.Cobble.Prelude hiding (ix)

import Language.Cobble.Shared

import qualified Data.Text as T

import Language.Cobble.MCAsm.Types

import Language.Cobble.MCAsm.McFunction

initialize :: (CompC r) => Sem r IntermediateResult
initialize = InterModule "init" . pure . InterInstructions . catMaybes <$> sequenceA [
      pure $ Just $ addScoreboardObjective regs
    , pure $ Just $ addScoreboardObjective eptr
    , pure $ Just $ addScoreboardObjective uid
    , pure $ Just $ addScoreboardObjective aptr
    , pure $ Just $ addScoreboardObjective ix
    , pure $ Just $ setScoreboardForPlayer uid "UID" 0
    , whenDebugAlt $ pure $ Just $ setScoreboardSidebar regs
    ]

clean :: (CompC r) => Sem r IntermediateResult
clean = InterModule "init" . pure . InterInstructions . catMaybes <$> sequenceA [
      pure $ Just $ removeScoreboardObjective regs
    , pure $ Just $ removeScoreboardObjective eptr
    , pure $ Just $ removeScoreboardObjective uid
    , pure $ Just $ removeScoreboardObjective aptr
    , pure $ Just $ removeScoreboardObjective ix
    ]

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
               . concat <$> mapM (\(n, inner) -> makeModulesInner (mname <> "/" <> n) inner) mods
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
    MoveNumLit reg lit -> instr [setScoreboardForPlayer regs (renderReg reg) lit]
    MoveNumReg reg1 reg2 -> instr [moveReg regs reg1 regs reg2]
    AddLit reg lit -> instr [addReg reg lit]
    AddReg reg1 reg2 -> instr [regOperation reg1 SAdd reg2]
    SubLit reg lit -> instr [subReg reg lit]
    DivLit reg lit -> instr $ opLit reg SDiv lit
    Section name instrs -> pure . InterModule name . concat <$> (mapM compileInstr instrs)
    Call section -> instr [runFunction ns section]
    CallEq reg1 reg2 section -> instr [
          setReg elseReg 1
        , execute $ EIf (eiScoreReg reg1 $ eiEqReg reg2) $ ERun $ runFunction ns section
        ]
    CallElse section -> instr [
          execute $ EIf (eiScoreReg elseReg $ EIMatches (RBounded 1 1)) $ ERun $ runFunction ns section
        ]
    Then -> instr [setReg elseReg 0]
    GetCommandResult reg command -> instr [
          execute $ EStoreRes (stScoreReg reg) $ ERun $ rawCommand command
        ]
    GetBySelector reg selector -> instr [
          execute $ EAs "@e" $ EIf (EIScore eptr "@s" $ EIEQ eptr (renderReg reg)) $ ERun $ resetScoreboardForPlayer eptr "@s"
        , incrementUID
        , execute $ EAs ("@e[" <> selector <> "]") $ ERun $ scoreboardOperation eptr "@s" SAssign uid "UID"
        , scoreboardOperation eptr (renderReg reg) SAssign uid "UID"
        ]
    RunCommandAsEntity command reg -> instr [
          execute $ EAs "@e" $ EIf (EIScore eptr "@s" $ EIEQ eptr (renderReg reg)) $ ERun $ rawCommand command
        ]
    GetNumInArray reg arr aix -> instr [
          asArrayElem arr aix $ scoreboardOperation regs (renderReg reg) SAssign regs "@s" 
        ]

    GetEntityInArray reg arr aix -> instr [
          asArrayElem arr aix $ scoreboardOperation eptr (renderReg reg) SAssign eptr "@s"
        ]
    SetNumInArray arr aix reg -> instr $
          asArrayElemOrNew arr aix $ scoreboardOperation regs "@s" SAssign regs (renderReg reg)
        
    SetEntityInArray arr aix reg -> instr $
          asArrayElemOrNew arr aix $ scoreboardOperation eptr "@s" SAssign eptr (renderReg reg)
        
    SetScoreboard obj player reg -> instr [
          scoreboardOperation obj player SAssign regs (renderReg reg)
        ]
    MoveArray r1 r2 -> instr [
          moveReg aptr r1 aptr r2
        ]
    GetArrayInArray reg arr aix -> instr [
          asArrayElem arr aix $ moveScoreboard aptr (renderReg reg) aptr "@s"
        ]
    SetArrayInArray arr aix reg -> instr $
          asArrayElemOrNew arr aix $ moveScoreboard aptr "@s" aptr (renderReg reg)
            
        
    where
        instr :: (Applicative f) => [McFunction] -> f [IntermediateResult]
        instr = pure . pure . InterInstructions
        opLit :: Register 'Number -> SOperation -> Int -> [McFunction]
        opLit r s l = [
              setReg constReg l
            , regOperation r s constReg
            ]
        incrementUID :: McFunction
        incrementUID = addScoreboardForPlayer uid "UID" 1
        incrementCompUID :: CompC r => Sem r Int
        incrementCompUID = gets compUID <* modify (\s -> s{compUID = compUID s + 1})

asArrayElemOrNew :: Register 'Array -> Register 'Number -> McFunction -> [McFunction]
asArrayElemOrNew areg ixreg mcf = asArrayElemOrIfNotPresent areg ixreg mcf 
    [ summonMarkerWithTags [arrayTag, tempTag]
    , moveScoreboard aptr ("@e[tag=" <> renderTag tempTag <> "]") aptr (renderReg areg)
    , moveScoreboard ix ("@e[tag=" <> renderTag tempTag <> "]") regs (renderReg ixreg)
    , mcf
    ]

asArrayElemOrIfNotPresent :: Register 'Array -> Register 'Number -> McFunction -> [McFunction] -> [McFunction]
asArrayElemOrIfNotPresent areg ixreg mcf inp = [
      setReg elseReg 0 
    , asArrayElem areg ixreg $ setReg elseReg 1
    , execute $ EIf (eiScoreReg elseReg $ EIMatches (RBounded 1 1)) $ ERun $ asArrayElem areg ixreg mcf
    ] <> map (execute . EIf (eiScoreReg elseReg $ EIMatches (RBounded 0 0)) . ERun) inp

asArrayElem :: Register 'Array -> Register 'Number -> McFunction -> McFunction
asArrayElem areg ixreg mcf = execute $ EAs "@e[tag=ARRAY]" 
                        $ EIf (EIScore aptr "@s" $ EIEQ aptr (renderReg areg))
                        $ EIf (EIScore ix "@s" $ EIEQ ix (renderReg ixreg))
                        $ ERun mcf

whenDebugMonoid :: (CompC r, Monoid a) => Sem r a -> Sem r a
whenDebugMonoid s = asks debug >>= bool (pure mempty) s

whenDebugAlt ::  (CompC r, Alternative f) => Sem r (f a) -> Sem r (f a)
whenDebugAlt s = asks debug >>= bool (pure empty) s

constReg :: Register 'Number
constReg = CustomReg "CONST"

elseReg :: Register 'Number
elseReg = CustomReg "ELSE"

arrayCounterReg :: Register 'Number
arrayCounterReg = CustomReg "ARRAYCOUNTER"

regs :: Objective
regs = Objective "REGS"

aptr :: Objective
aptr = Objective "APTR"

ix :: Objective
ix = Objective "IX"

eptr :: Objective
eptr = Objective "EPTR"

uid :: Objective
uid = Objective "UID"

tempTag :: Tag
tempTag = Tag "TEMP"

arrayTag :: Tag
arrayTag = Tag "ARRAY"

setReg :: Register 'Number -> Int -> McFunction
setReg = setScoreboardForPlayer regs . renderReg

addReg :: Register 'Number -> Int -> McFunction
addReg r = addScoreboardForPlayer regs (renderReg r)

subReg :: Register 'Number -> Int -> McFunction
subReg r = subScoreboardForPlayer regs (renderReg r)

eiScoreReg :: Register 'Number -> EIScoreOp -> EIfParam
eiScoreReg r = EIScore regs (renderReg r)

eiEqReg :: Register 'Number -> EIScoreOp
eiEqReg r = EIEQ regs (renderReg r)

stScoreReg :: Register 'Number -> Store
stScoreReg = StScore regs . renderReg

regOperation :: Register 'Number -> SOperation -> Register 'Number -> McFunction
regOperation r1 o r2 = scoreboardOperation regs (renderReg r1) o regs (renderReg r2)

moveReg :: Objective -> Register a -> Objective -> Register b -> McFunction
moveReg o1 r1 o2 r2 = moveScoreboard o1 (renderReg r1) o2 (renderReg r2)

comment :: Text -> IntermediateResult
comment = InterInstructions . pure . McFunction . ("#" <>)
