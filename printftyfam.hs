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

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Symbol.Ascii (ToList)
import GHC.TypeLits (AppendSymbol, KnownSymbol, Symbol, symbolVal)

data FormatType a where
  Fint :: FormatType Int
  Fstr :: FormatType String

showFormatted :: FormatType a -> a -> String
showFormatted Fint x = show x
showFormatted Fstr x = x

data Format a where
  FNil :: Format String
  (:%) :: FormatType a -> Format b -> Format (a -> b)
  (:>) :: String -> Format a -> Format a

infixr 9 :%

infixr 9 :>

printfFormat :: Format a -> a
printfFormat = go []
  where
    go :: [String] -> Format a -> a
    go acc FNil = concat $ reverse acc
    go acc (pa :> rest) = go (pa : acc) rest
    go acc (pa :% rest) =
      \x -> go (showFormatted pa x : acc) rest

data FormatArg
  = LitArg Symbol
  | IntArg
  | StringArg

type family Parse (str :: Symbol) :: [FormatArg] where
  Parse str = ToFormatArgs (ToList str)

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

type family ToFormatArgs (syms :: [Symbol]) :: [FormatArg] where
  ToFormatArgs '[] = '[]
  ToFormatArgs ("%" ': "d" ': str) = IntArg ': ToFormatArgs str
  ToFormatArgs ("%" ': "s" ': str) = StringArg ': ToFormatArgs str
  ToFormatArgs str =
    LitArg (Fst (SpanFormat str)) ': ToFormatArgs (Snd (SpanFormat str))

type family ResFormatArgsToFormat (format :: [FormatArg]) :: Type where
  ResFormatArgsToFormat (IntArg ': rest) = Int -> ResFormatArgsToFormat rest
  ResFormatArgsToFormat (StringArg ': rest) = String -> ResFormatArgsToFormat rest
  ResFormatArgsToFormat (LitArg _ ': rest) = ResFormatArgsToFormat rest
  ResFormatArgsToFormat '[] = String

class FormatArgsToFormat (format :: [FormatArg]) where
  fmt :: Format (ResFormatArgsToFormat format)

instance FormatArgsToFormat rest => FormatArgsToFormat (IntArg ': rest) where
  fmt = Fint :% fmt @rest

instance FormatArgsToFormat rest => FormatArgsToFormat (StringArg ': rest) where
  fmt = Fstr :% fmt @rest

instance (FormatArgsToFormat rest, KnownSymbol sym) => FormatArgsToFormat (LitArg sym ': rest) where
  fmt = symbolVal (Proxy @sym) :> fmt @rest

instance FormatArgsToFormat '[] where
  fmt = FNil

printf :: forall sym res. FormatArgsToFormat (Parse sym) => ResFormatArgsToFormat (Parse sym)
printf = printfFormat $ fmt @(Parse sym)
