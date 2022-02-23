{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Kind
import Data.Proxy (Proxy (Proxy))
import Data.Symbol.Ascii
import GHC.TypeLits

data FormatArg
  = Lit Symbol
  | FInt
  | FString

type family Parse (str :: Symbol) :: [FormatArg] where
  Parse str = ToFormatArgs (GroupFormats (ToList str))

type family ToFormatArgs (syms :: [Symbol]) :: [FormatArg] where
  ToFormatArgs '[] = '[]
  ToFormatArgs ("%v" ': str) = FInt ': ToFormatArgs str
  ToFormatArgs ("%s" ': str) = FString ': ToFormatArgs str
  ToFormatArgs (s ': str) = Lit s ': ToFormatArgs str

type family Fst (tup :: (a, b)) where
  Fst '(x, y) = x

type family Snd (tup :: (a, b)) where
  Snd '(x, y) = y

type family SpanFormat (syms :: [Symbol]) :: (Symbol, [Symbol]) where
  SpanFormat '[] = '("", '[])
  SpanFormat ("%" ': c ': str) = '("", "%" ': c ': str)
  SpanFormat (x ': xs) =
    '( AppendSymbol x (Fst (SpanFormat xs)),
       Snd (SpanFormat xs)
     )

type family GroupFormats (syms :: [Symbol]) :: [Symbol] where
  GroupFormats '[] = '[]
  GroupFormats ("%" ': c ': str) = AppendSymbol "%" c ': GroupFormats str
  GroupFormats str =
    Fst (SpanFormat str) ': GroupFormats (Snd (SpanFormat str))

class FormatBla (format :: [FormatArg]) res | format -> res where
  fmt :: Format res

instance FormatBla rest res' => FormatBla (FInt ': rest) (Int -> res') where
  fmt = Fint :% fmt @rest

instance FormatBla rest res' => FormatBla (FString ': rest) (String -> res') where
  fmt = Fstr :% fmt @rest

instance (FormatBla rest res', KnownSymbol sym) => FormatBla (Lit sym ': rest) res' where
  fmt = symbolVal (Proxy @sym) :> fmt @rest

instance FormatBla '[] String where
  fmt = FNil

printf' :: forall sym res. FormatBla (Parse sym) res => res
printf' = printf $ fmt @(Parse sym)

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
