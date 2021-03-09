{-# LANGUAGE NoImplicitPrelude, DataKinds, BlockArguments, LambdaCase #-}
{-# LANGUAGE NamedFieldPuns, ConstraintKinds, OverloadedStrings #-}
module Language.Cobble.Parser.Preprocessor where

{- Idea:

    Tokenize
 -> Look through tokens and find all ([Reserved "defmacro", Ident x])       done (needs test)
 -> for every x, replace all tokens (Ident y) with (MacroCall x)            done (needs test)
 -> Parse Tokens (context free), keeping macro arguments as tokens          todo (Happy)
 -> extract macro "types"                                                   todo
 -> Parse Macro Arguments (context free, since types should now be known)   todo
 -> Expand macros with parsed arguments                                     todo
 
 -> Typecheck                                                               done (kinda)
 
 -> Codegen                                                                 done (kinda)
 
 -> Assemble                                                                done (kinda)

-}

import Language.Cobble.Prelude

import Language.Cobble.Types
import Language.Cobble.Parser.Tokenizer

data PreprocessError = MacroNameNotIdent (Token 'Unprocessed) deriving (Show, Eq)

type PreprocessC r = Members '[Error PreprocessError] r

-- TODO: Scope?
-- TODO: Tests!

preProcess ::[Token 'Unprocessed] -> Either PreprocessError [Token 'Processed]
preProcess toks = run $ runError $ findMacroNames toks <&> replaceMacroCalls toks

findMacroNames :: (PreprocessC r) => [Token 'Unprocessed] -> Sem r [Name 'ParsePreprocess]
findMacroNames [] = pure []
findMacroNames ((Token _ (Reserved "defmacro")) : (Token _ (Ident name)) : xs) = (name:) <$> findMacroNames xs
findMacroNames ((Token _ (Reserved "defmacro") : t : _)) = throw $ MacroNameNotIdent t
findMacroNames (_:xs) = findMacroNames xs

replaceMacroCalls :: [Token 'Unprocessed] -> [Name 'ParsePreprocess] -> [Token 'Processed]
replaceMacroCalls ts names = ts & map \case
    (Token l (Ident x)) | x `elem` names -> Token l (MacroCall x)
    x -> coerce x
