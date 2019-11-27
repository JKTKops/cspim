{-# LANGUAGE OverloadedStrings #-}
module Pretty
    ( module Pretty
    , Data.Text.Text
    )where

import Data.Text

class Pretty a where
    pretty :: a -> Text
    prettyList :: [a] -> Text

    prettyList [] = "[]"
    prettyList as = "[" <> go as
      where go [] = "]"
            go (a:as) = pretty a <> ", " <> go as

instance Pretty a => Pretty [a] where
    pretty = prettyList

instance Pretty Char where
    pretty = singleton
    prettyList = pack
