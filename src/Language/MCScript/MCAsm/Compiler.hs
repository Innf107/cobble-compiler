{-# LANGUAGE NoImplicitPrelude, LambdaCase, DataKinds, ConstraintKinds, OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}
module Language.MCScript.MCAsm.Compiler where

import Language.MCScript.Prelude

import qualified Data.Text as T

import Language.MCScript.MCAsm.Types

initialize :: (CompC r) => Sem r IntermediateResult
initialize = InterModule "init" . pure . InterInstructions . T.unlines <$> sequenceA [
      pure $ summonASWithTags ["STACK"]
    , pure $ "scoreboard objectives add REGS dummy"
    , pure $ "scoreboard objectives add EPTR dummy"
    , pure $ "scoreboard objectives add UID dummy"
    , pure $ "scoreboard objectives add APTR dummy"
    , pure $ "scoreboard objectives add IX dummy"
    , pure $ "scoreboard objectives set UID UID 0"
    , whenDebugMonoid $ pure "scoreboard objectives setdisplay sidebar REGS"
    ]

clean :: (CompC r) => Sem r IntermediateResult
clean = InterModule "clean" . pure . InterInstructions . T.unlines <$> sequenceA [
      pure $ "kill @e[tag=STACK]"
    , pure $ "scoreboard objectives remove REGS"
    , pure $ "scoreboard objectives remove EPTR"
    , pure $ "scoreboard objectives remove UID"
    , pure $ "scoreboard objectives remove APTR"
    , pure $ "scoreboard objectives remove IX"
    ]

hoistModules :: (CompC r) => IntermediateResult -> Sem r [CompiledModule]
hoistModules (InterInstructions _) = error "Cannot create modules from standalone top level instructions"
hoistModules (InterModule mn ins) = makeModulesInner mn ins
    where
        makeModulesInner :: Text -> [IntermediateResult] -> Sem r [CompiledModule]
        makeModulesInner mname subInstrs =
            let (mods, instrs) = partitionEithers $
                    map (\case
                        InterModule n rs -> Left (n, rs)
                        InterInstructions is -> Right is
                    ) subInstrs
            in (CompiledModule mname (unlines instrs):)
               . concat <$> mapM (\(n, inner) -> makeModulesInner (mname <> "/" <> n) inner) mods

compile :: (CompC r) => [Module] -> Sem r [CompiledModule]
compile mods = fmap concat $ join $ mapM hoistModules <$>
    ((++) <$> sequenceA [initialize,clean] <*> mapM compileModule mods)

compileModule :: (CompC r) => Module -> Sem r IntermediateResult
compileModule (Module {moduleName, moduleInstructions}) = do
    results <- mapM compileInstr $ moduleInstructions
    pure $ InterModule moduleName (concat results)

compileInstr :: (CompC r) => Instruction -> Sem r [IntermediateResult]
compileInstr i = asks nameSpace >>= \ns ->
    let call f = "function " <> ns <> ":" <> f in case i of
    MoveNumLit reg lit -> instr ["scoreboard players set " <> renderReg reg <> " REGS " <> show lit]
    MoveNumReg reg1 reg2 -> instr ["scoreboard players operation " <> renderReg reg1 <> " REGS = " <>
                            renderReg reg2 <> " REGS"]
    AddLit reg lit -> opLit "+=" reg lit
    AddReg reg1 reg2 -> instr [ "scoreboard players operation " <> renderReg reg1 <> " REGS += " <> renderReg reg2 <> "REGS"]
    SubLit reg lit -> opLit "-=" reg lit
    DivLit reg lit -> opLit "/=" reg lit
    PushLit lit -> instr [
          "execute positioned 0 0 0 at @e[tag=STACK, limit=1, sort=furthest] run summon armor_stand ~1 ~ ~ {Marker:1, Invisible:1, Tags:[\"STACK\"]}"
        , "execute positioned 0 0 0 as @e[tag=STACK, limit=1, sort=furthest] run scoreboard players set @s REGS " <> show lit
        ]
    PushReg reg -> instr [
          "execute positioned 0 0 0 at @e[tag=STACK, limit=1, sort=furthest] run summon armor_stand ~1 ~ ~ {Marker:1, Invisible:1, Tags:[\"STACK\"]}"
        , "execute positioned 0 0 0 as @e[tag=STACK, limit=1, sort=furthest] run scoreboard players operation @s REGS = " <> renderReg reg <> " REGS"
        ]
    PopNum reg -> instr [
          "execute positioned 0 0 0 as @e[tag=STACK, limit=1, sort=furthest] run scoreboard players operation " <> renderReg reg <> " REGS = @s REGS"
        , "execute positioned 0 0 0 as @e[tag=STACK, limit=1, sort=furthest] run kill @s"
        ]
    Section name instrs -> pure . InterModule name . concat <$> (mapM (compileInstr) instrs)
    Call section -> instr [call section]
    CallEq reg1 reg2 section -> instr [
          "scoreboard players set ELSE REGS 1"
        , "execute if score " <> renderReg reg1 <> " REGS = " <> renderReg reg2 <> " REGS run " <> call section
        ]
    CallElse section -> instr [
          "execute if score ELSE REGS matches 1 run " <> call section
        ]
    Then -> instr ["scoreboard players set ELSE REGS 0"]
    GetCommandResult reg command -> instr [
          "execute store result score " <> renderReg reg <> " REGS run " <> command
        ]
    PushE reg -> instr [
          "execute positioned 0 0 0 at @e[tag=STACK, limit=1, sort=furthest] run summon armor_stand ~1 ~ ~ {Marker:1, Invisible:1, Tags:[\"STACK\"]}"
        , "execute positioned 0 0 0 as @e[tag=STACK, limit=1, sort=furthest] run scoreboard players operation @s EPTR = " <> renderReg reg <> " EPTR"
        ]
    PopE reg -> instr [
          "execute positioned 0 0 0 as @e[tag=STACK, limit=1, sort=furthest] run scoreboard players operation " <> renderReg reg <> " EPTR = @s EPTR"
        , "execute positioned 0 0 0 as @e[tag=STACK, limit=1, sort=furthest] run kill @s"
        ]
    GetBySelector reg selector -> instr [
          "execute as @e if score @s EPTR = " <> renderReg reg <> " EPTR run scoreboard players reset @s EPTR"
        , incrementUID
        , "execute as @e[" <> selector <> "] run scoreboard players operation @s EPTR = UID UID"
        , "scoreboard players operation " <> renderReg reg <> " EPTR = UID UID"
        ]
    RunCommandAsEntity command reg -> instr [
          "execute as @e if score @s EPTR = " <> renderReg reg <> " EPTR run " <> command
        ]
    MakeArray reg len -> do
        modName <- ("create-array-" <>) . show <$> incrementCompUID
        pure [
              InterModule modName $ pure $ InterInstructions $ T.unlines [
                "scoreboard players remove ARRAYCOUNTER REGS 1"
              , "summon armor_stand 0 0 0 {Tags:[\"TEMP\", \"ARRAY\"]}"
              , "scoreboard players operation @e[tag=TEMP] APTR = UID UID"
              , "scoreboard players operation @e[tag=TEMP] IX = ARRAYCOUNTER REGS"
              , "tag @e[tag=TEMP] remove TEMP"
              , "execute if score ARRAYCOUNTER REGS matches 1.. run" <> call modName
              ]
            , InterInstructions $ T.unlines [
                incrementUID
              , "scoreboard players operation " <> renderReg reg <> " REGS = UID UID"
              , "scoreboard players operation ARRAYCOUNTER REGS = " <> renderReg len <> " REGS"
              , "execute if score ARRAYCOUNTER REGS matches 1.. run" <> call modName
              ]
            ]
    GetNumInArray reg arr aix -> instr [
          "execute as @e[tag=ARRAY] if score @s APTR = " <> renderReg arr <> " APTR if score @s IX = "
            <> renderReg aix <> " IX run scoreboard players operation " <> renderReg reg <> " REGS = @s REGS"
        ]

    GetEntityInArray reg arr aix -> instr [
        "execute as @e[tag=ARRAY] if score @s APTR = " <> renderReg arr <> " APTR if score @s IX = "
            <> renderReg aix <> " IX run scoreboard players operation " <> renderReg reg <> " EPTR = @s EPTR"
        ]

    SetNumInArray arr aix reg -> instr [
          "execute as @e[tag=ARRAY] if score @s APTR = " <> renderReg arr <> " APTR if score @s IX = "
            <> renderReg aix <> " IX run scoreboard players operation @s REGS = " <> renderReg reg <> " REGS"
        ]

    SetEntityInArray arr aix reg -> instr [
          "execute as @e[tag=ARRAY] if score @s APTR = " <> renderReg arr <> " APTR if score @s IX = "
            <> renderReg aix <> " IX run scoreboard players operation @s EPTR = " <> renderReg reg <> " EPTR"
        ]

    where
        instr :: (Applicative f) => [Text] -> f [IntermediateResult]
        instr = pure . pure . InterInstructions . T.unlines
        opLit :: (Applicative f) => Text -> (Register Number) -> Int -> f [IntermediateResult]
        opLit operator reg lit = instr $  [ "scoreboard players set CONST REGS " <> show lit
                    , "scoreboard players operation " <> renderReg reg <> " REGS " <> operator <> " CONST REGS"]
        incrementUID :: Text
        incrementUID = "scoreboard players add UID UID 1"
        incrementCompUID :: CompC r => Sem r Int
        incrementCompUID = gets compUID <* modify (\s -> s{compUID = compUID s + 1})

summonASWithTags :: [Text] -> Text
summonASWithTags = summonASAtWithTags "0" "0" "0"

summonASAtWithTags :: Text -> Text -> Text -> [Text] -> Text
summonASAtWithTags x y z tags = "summon minecraft:armor_stand " <> T.unwords [x, y, z]
                                <> " {Marker:1, Invisible:1, Tags:["
                                <> T.intercalate "," (map show tags)
                                <> "]}"

whenDebugMonoid :: (CompC r, Monoid a) => Sem r a -> Sem r a
whenDebugMonoid s = asks debug >>= bool (pure mempty) s
