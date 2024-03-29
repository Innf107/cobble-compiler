module Cobble.Prelude.Parser (
        module Exports
    ,   many, sepBy, sepBy1
    ) where

import Cobble.Prelude as Exports hiding (optional, noneOf, try, many)

import Text.Parsec qualified as P
import Text.Parsec as Exports hiding (
        (<|>), State, Empty, uncons
    ,   many, sepBy, sepBy1
    )
import Text.Parsec.Pos as Exports

many :: ParsecT s u m a -> ParsecT s u m (Seq a)
many = fmap fromList . P.many

sepBy :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m (Seq a)
sepBy p s = fromList <$> P.sepBy p s

sepBy1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m (Seq a)
sepBy1 p s = fromList <$> P.sepBy1 p s
