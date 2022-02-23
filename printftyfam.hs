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

class FormatBla (format :: [FormatArg]) where
  type Res format
  fmt :: Format (Res format)

instance FormatBla rest => FormatBla (FInt ': rest) where
  type Res (FInt ': rest) = Int -> Res rest
  fmt = Fint :% fmt @rest

instance FormatBla rest => FormatBla (FString ': rest) where
  type Res (FString ': rest) = String -> Res rest
  fmt = Fstr :% fmt @rest

instance (FormatBla rest, KnownSymbol sym) => FormatBla (Lit sym ': rest) where
  type Res (Lit sym ': rest) = Res rest
  fmt = symbolVal (Proxy @sym) :> fmt @rest

instance FormatBla '[] where
  type Res '[] = String
  fmt = FNil

printf' :: forall sym res. FormatBla (Parse sym) => Res (Parse sym)
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
