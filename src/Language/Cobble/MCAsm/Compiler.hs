{-# LANGUAGE NoImplicitPrelude, LambdaCase, DataKinds, ConstraintKinds, OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}
module Language.Cobble.MCAsm.Compiler where

import Language.Cobble.Prelude

import Language.Cobble.Shared

import qualified Data.Text as T

import Language.Cobble.MCAsm.Types

import Language.Cobble.MCAsm.McFunction

initialize :: (CompC r) => Sem r IntermediateResult
initialize = InterModule "init" . pure . InterInstructions . catMaybes <$> sequenceA [
      pure $ Just $ addScoreboardObjective "REGS"
    , pure $ Just $ addScoreboardObjective "EPTR"
    , pure $ Just $ addScoreboardObjective "UID"
    , pure $ Just $ addScoreboardObjective "APTR"
    , pure $ Just $ addScoreboardObjective "IX"
    , pure $ Just $ setScoreboardForPlayer "UID" "UID" 0
    , whenDebugAlt $ pure $ Just $ setScoreboardSidebar "REGS"
    ]

clean :: (CompC r) => Sem r IntermediateResult
clean = InterModule "init" . pure . InterInstructions . catMaybes <$> sequenceA [
      pure $ Just $ removeScoreboardObjective "REGS"
    , pure $ Just $ removeScoreboardObjective "EPTR"
    , pure $ Just $ removeScoreboardObjective "UID"
    , pure $ Just $ removeScoreboardObjective "APTR"
    , pure $ Just $ removeScoreboardObjective "IX"
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
compileInstr i = asks nameSpace >>= \ns -> case i of
    MoveNumLit reg lit -> instr [setScoreboardForPlayer "REGS" (renderReg reg) lit]
    MoveNumReg reg1 reg2 -> instr [regOperation reg1 SAssign reg2]
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
          execute $ EAs "@e" $ EIf (EIScore "EPTR" "@s" $ EIEQ "EPTR" (renderReg reg)) $ ERun $ resetScoreboardForPlayer "EPTR" "@s"
        , incrementUID
        , execute $ EAs ("@e[" <> selector <> "]") $ ERun $ scoreboardOperation "EPTR" "@s" SAssign "UID" "UID"
        , scoreboardOperation "EPTR" (renderReg reg) SAssign "UID" "UID"
        ]
    RunCommandAsEntity command reg -> instr [
          execute $ EAs "@e" $ EIf (EIScore "EPTR" "@s" $ EIEQ "EPTR" (renderReg reg)) $ ERun $ rawCommand command
        ]
    MakeArray reg len -> do
        modName <- (prependQual "create-array") . show <$> incrementCompUID
        pure [
              InterModule modName $ pure $ InterInstructions [
                subReg arrayCounterReg 1
              , summonMarkerWithTags ["TEMP", "ARRAY"]
              , scoreboardOperation "APTR" "@e[tag=TEMP]" SAssign "UID" "UID"
              , scoreboardOperation "IX" "@e[tag=TEMP]" SAssign "REGS" (renderReg arrayCounterReg)
              , removeTag "TEMP" "@e[tag=TEMP]"
              , execute $ EIf (eiScoreReg arrayCounterReg $ EIMatches (RInfEnd 1)) $ ERun $ runFunction ns modName
              ]
            , InterInstructions [
                incrementUID
              , scoreboardOperation "REGS" (renderReg reg) SAssign "UID" "UID"
              , regOperation arrayCounterReg SAssign len
              , execute $ EIf (eiScoreReg arrayCounterReg $ EIMatches (RInfEnd 1)) $ ERun $ runFunction ns modName
              ]
            ]
    GetNumInArray reg arr aix -> instr [
          execute $ EAs "@e[tag=ARRAY]" 
            $ EIf (EIScore "APTR" "@s" $ EIEQ "APTR" (renderReg arr)) 
            $ EIf (EIScore "IX" "@s" $ EIEQ "IX" (renderReg aix)) 
            $ ERun $ scoreboardOperation "REGS" (renderReg reg) SAssign "REGS" "@s" 
        ]

    GetEntityInArray reg arr aix -> instr [
          execute $ EAs "@e[tag=ARRAY]" 
            $ EIf (EIScore "APTR" "@s" $ EIEQ "APTR" (renderReg arr)) 
            $ EIf (EIScore "IX" "@s" $ EIEQ "IX" (renderReg aix)) 
            $ ERun $ scoreboardOperation "EPTR" (renderReg reg) SAssign "EPTR" "@s"
        ]

    SetNumInArray arr aix reg -> instr [
          execute $ EAs "@e[tag=ARRAY]" 
            $ EIf (EIScore "APTR" "@s" $ EIEQ "APTR" (renderReg arr)) 
            $ EIf (EIScore "IX" "@s" $ EIEQ "IX" (renderReg aix))
            $ ERun $ scoreboardOperation "REGS" "@s" SAssign "REGS" (renderReg reg)
        ]

    SetEntityInArray arr aix reg -> instr [
          execute $ EAs "@e[tag=ARRAY]" 
            $ EIf (EIScore "APTR" "@s" $ EIEQ "APTR" (renderReg arr)) 
            $ EIf (EIScore "IX" "@s" $ EIEQ "IX" (renderReg aix))
            $ ERun $ scoreboardOperation "EPTR" "@s" SAssign "EPTR" (renderReg reg)
        ]
    SetScoreboard obj player reg -> instr [
          scoreboardOperation obj player SAssign "REGS" (renderReg reg)
        ]

    where
        instr :: (Applicative f) => [McFunction] -> f [IntermediateResult]
        instr = pure . pure . InterInstructions
        opLit :: Register 'Number -> SOperation -> Int -> [McFunction]
        opLit r s l = [
              setReg constReg l
            , regOperation r s constReg
            ]
        incrementUID :: McFunction
        incrementUID = addScoreboardForPlayer "UID" "UID" 1
        incrementCompUID :: CompC r => Sem r Int
        incrementCompUID = gets compUID <* modify (\s -> s{compUID = compUID s + 1})


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

setReg :: Register 'Number -> Int -> McFunction
setReg = setScoreboardForPlayer "REGS" . renderReg

addReg :: Register 'Number -> Int -> McFunction
addReg r = addScoreboardForPlayer "REGS" (renderReg r)

subReg :: Register 'Number -> Int -> McFunction
subReg r = subScoreboardForPlayer "REGS" (renderReg r)

eiScoreReg :: Register 'Number -> EIScoreOp -> EIfParam
eiScoreReg r = EIScore "REGS" (renderReg r)

eiEqReg :: Register 'Number -> EIScoreOp
eiEqReg r = EIEQ "REGS" (renderReg r)

stScoreReg :: Register 'Number -> Store
stScoreReg = StScore "REGS" . renderReg

regOperation :: Register 'Number -> SOperation -> Register 'Number -> McFunction
regOperation r1 o r2 = scoreboardOperation "REGS" (renderReg r1) o "REGS" (renderReg r2)
