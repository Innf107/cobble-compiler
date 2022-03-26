module Language.Cobble.Prelude.Parser (
        module Exports
    ,   many, sepBy1
    ) where

import Language.Cobble.Prelude as Exports hiding (optional, noneOf, try, many)

import Text.Parsec qualified as P
import Text.Parsec as Exports hiding (
        (<|>), State, Empty, uncons
    ,   many, sepBy1
    )
import Text.Parsec.Pos as Exports

many :: ParsecT s u m a -> ParsecT s u m (Seq a)
many = fmap fromList . P.many


sepBy1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m (Seq a)
sepBy1 p s = fromList <$> P.sepBy1 p s
