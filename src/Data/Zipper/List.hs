{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Zipper.List where

import qualified GHC.Exts as E (IsList(..))
import Control.Arrow ((&&&))
import Control.Monad.Identity
import Data.Function ((&))
import Data.Functor  ((<&>))

data Zipper a = Zip [a] [a]
  deriving (Eq, Functor)

toList :: Zipper a -> [a]
toList (Zip crumbs lst) = go crumbs lst
  where go :: [a] -> [a] -> [a]
        go = flip (foldl (flip (:)))

fromList :: [a] -> Zipper a
fromList = Zip []

right :: Zipper a -> Maybe (Zipper a)
right (Zip _ []) = Nothing
right (Zip cs (x:xs)) = Just $ Zip (x:cs) xs

left :: Zipper a -> Maybe (Zipper a)
left (Zip [] _) = Nothing
left (Zip (c:cs) xs) = Just $ Zip cs (c:xs)

current :: Zipper a -> Maybe a
current (Zip _ []) = Nothing
current (Zip _ (x:_)) = Just x

start :: Zipper a -> Zipper a
start = fromList . toList

-- | One /past/ the last item in the zipper.
end :: Zipper a -> Zipper a
end (Zip cs lst) = Zip (go cs lst) []
  where go = foldl (flip (:))

delete :: Zipper a -> Zipper a
delete z@(Zip _ []) = z
delete (Zip cs (_:xs)) = Zip cs xs

insert :: a -> Zipper a -> Zipper a
insert x (Zip cs xs) = Zip cs (x:xs)

remove :: Zipper a -> (Zipper a, Maybe a)
remove = delete &&& current

reverse :: Zipper a -> Zipper a
reverse (Zip cs []) = Zip [] cs
reverse (Zip cs (x:xs)) = Zip xs (x:cs)

isStart :: Zipper a -> Bool
isStart (Zip [] _) = True
isStart _ = False

isEnd :: Zipper a -> Bool
isEnd (Zip _ []) = True
isEnd _ = False

isLast :: Zipper a -> Bool
isLast (Zip _ [_]) = True
isLast _ = False

-- | Take elements from the current position of the zipper.
take :: Int -> Zipper a -> [a]
take n (Zip _ xs) = Prelude.take n xs

deleteMany :: Int -> Zipper a -> Zipper a
deleteMany n z = foldl (&) z $ replicate n delete

instance E.IsList (Zipper a) where
    type Item (Zipper a) = a
    toList = toList
    fromList = fromList

instance Show a => Show (Zipper a) where
    show zip = "fromList " ++ show (toList zip)

instance Foldable Zipper where
    foldMap f (Zip cs xs) = foldMap f (Prelude.reverse cs) `mappend` foldMap f xs

instance Traversable Zipper where
    traverse f (Zip cs xs) =
        Zip <$> (Prelude.reverse <$> traverse f (Prelude.reverse cs))
            <*> traverse f xs

duplicate :: Zipper a -> Zipper (Zipper a)
duplicate z = Zip (maybe [] (dupWith left) $ left z)
                  (init $ dupWith right z) -- dupWith extends to the end, where there is no
                                           -- focus, so we drop that element via init
  where
    dupWith f r = case f r of
        Nothing -> [r]
        Just x  -> r : dupWith f x

atEvery :: (Zipper a -> b) -> Zipper a -> Zipper b
atEvery f = fmap f . duplicate

-- Note: fromJust . current, duplicate forms a non-total comonad
-- (atEvery is extend)

-- | Transform a zipper. The transformer takes a zipper with an arbitrary focus
-- and decides whether to modify it. If it makes a modification, it returns Just of a new
-- zipper. The returned zipper is the next argument to the transformer.
--
-- The transformer is applied until the transformer returns Nothing.
--
-- This function is a special case of unfoldrM.
transformM :: Monad m => (Zipper a -> m (Maybe (Zipper a))) -> Zipper a -> m (Zipper a)
transformM t = loop
  where
    loop z = t z >>= \case
        Nothing -> return z
        Just z' -> transformM t z'

transform :: (Zipper a -> Maybe (Zipper a)) -> Zipper a -> Zipper a
transform t = runIdentity . transformM (Identity . t)
