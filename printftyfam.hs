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
  = LitArg Symbol
  | IntArg
  | StringArg

type family Parse (str :: Symbol) :: [FormatArg] where
  Parse str = ToFormatArgs (GroupFormats (ToList str))

type family ToFormatArgs (syms :: [Symbol]) :: [FormatArg] where
  ToFormatArgs '[] = '[]
  ToFormatArgs ("%v" ': str) = IntArg ': ToFormatArgs str
  ToFormatArgs ("%s" ': str) = StringArg ': ToFormatArgs str
  ToFormatArgs (s ': str) = LitArg s ': ToFormatArgs str

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

type family ResFormatBla (format :: [FormatArg]) :: Type where
  ResFormatBla (IntArg ': rest) = Int -> ResFormatBla rest
  ResFormatBla (StringArg ': rest) = String -> ResFormatBla rest
  ResFormatBla (LitArg _ ': rest) = ResFormatBla rest
  ResFormatBla '[] = String

class FormatBla (format :: [FormatArg]) where
  fmt :: Format (ResFormatBla format)

instance FormatBla rest => FormatBla (IntArg ': rest) where
  fmt = Fint :% fmt @rest

instance FormatBla rest => FormatBla (StringArg ': rest) where
  fmt = Fstr :% fmt @rest

instance (FormatBla rest, KnownSymbol sym) => FormatBla (LitArg sym ': rest) where
  fmt = symbolVal (Proxy @sym) :> fmt @rest

instance FormatBla '[] where
  fmt = FNil

printf' :: forall sym res. FormatBla (Parse sym) => ResFormatBla (Parse sym)
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
