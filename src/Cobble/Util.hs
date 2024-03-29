{-# LANGUAGE NoOverloadedLists #-}
module Cobble.Util where

import Cobble.Prelude
  
import System.Directory

mapCompose ::  (f (g a) -> f' (g' b)) -> Compose f g a -> Compose f' g' b
mapCompose f = Compose . f . getCompose

todo :: a -> a
todo x = x
{-# WARNING todo "TODO" #-}

unsafeTail :: HasCallStack => NonEmpty a -> NonEmpty a
unsafeTail (_ :| []) = error "unsafeTail: only one element"
unsafeTail (_ :| (x:xs)) = x :| xs 

(<<|>>) :: (Applicative f, Alternative g) => f (g b) -> f (g b) -> f (g b) 
(<<|>>)= liftA2 (<|>)

zipFor :: [a] -> [b] -> (a -> b -> c) -> [c]
zipFor x y f = zipWith f x y

zipForM :: (Applicative f) => [a] -> [b] -> (a -> b -> f c) -> f [c]
zipForM x y f = zipWithM f x y

zipForM_ :: (Applicative f) => [a] -> [b] -> (a -> b -> f c) -> f ()
zipForM_ x y f = zipWithM_ f x y

copyFileOrDirectory :: Bool -> FilePath -> FilePath -> IO ()
copyFileOrDirectory parents from to =
    doesFileExist from >>= \case
        True -> copyFile from to
        False -> doesDirectoryExist from >>= \case
            True -> do
                createDirectoryIfMissing parents to
                files <- filter (`notElem` ["..", "."]) <$> getDirectoryContents from
                forM_ files $ \file -> copyFileOrDirectory parents (from </> file) (to </> file)
            False -> fail $ "copyFileOrDirectory: File does not exist: " <> from

lookupAndDeleteWith :: (a -> Maybe b) -> Seq a -> Maybe (b, Seq a)
lookupAndDeleteWith f Empty = Nothing
lookupAndDeleteWith f (a :<| as) = case f a of
    Just b -> Just (b, as)
    Nothing -> second (a <|) <$> lookupAndDeleteWith f as

lookupAndDelete :: (Eq a) => a -> Seq (a, b) -> Maybe (b, Seq (a, b))
lookupAndDelete x = lookupAndDeleteWith (\(x', y) -> if x == x' then Just y else Nothing)


-- Applies a monadic operation pairwise to every element from left to right and ignores the result
-- Example:
-- > pairwiseM_ f [1, 2, 3, 4] = f 1 2 >> f 2 3 >> f 3 4
pairwiseM_ :: Monad m => (a -> a -> m ()) -> Seq a -> m ()
pairwiseM_ f Empty = pure ()
pairwiseM_ f (x :<| y :<| ys) = f x y >> pairwiseM_ f (y :<| ys)
pairwiseM_ f (x :<| _) = pure ()

toSeq :: Foldable f => f a -> Seq a
toSeq = fromList . toList
