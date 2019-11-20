module Pretty where

class Pretty a where
    pretty :: a -> String
    prettyList :: [a] -> String

    prettyList [] = "[]"
    prettyList as = "[" ++ go as
      where go [] = "]"
            go (a:as) = pretty a ++ ", " ++ go as

instance Pretty a => Pretty [a] where
    pretty = prettyList
