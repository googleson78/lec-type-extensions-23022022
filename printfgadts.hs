{-# LANGUAGE GADTs #-}

data FormatV a where
  Fint :: FormatV Int
  Fstr :: FormatV String

showFormatted :: FormatV a -> a -> String
showFormatted Fint x = show x
showFormatted Fstr x = x

data Format a where
  FNil :: Format String
  (:%) :: FormatV a -> Format b -> Format (a -> b)
  (:>) :: String -> Format a -> Format a

infixr 9 :%

infixr 9 :>

printf :: Format a -> a
printf = go []
  where
    go :: [String] -> Format a -> a
    go acc FNil = concat $ reverse acc
    go acc (pa :> rest) = go (pa : acc) rest
    go acc (pa :% rest) =
      \x -> go (showFormatted pa x : acc) rest
