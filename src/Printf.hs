{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Printf where

import Data.Kind
import Data.Symbol.Ascii
import GHC.TypeLits (AppendSymbol, Symbol)

-- %d %s
-- printf :: String -> ... -> String

-- GADT - generalised algebraic data types

--data Foo = Bar Int | Qux String
--data Foo where
--  Bar :: Int -> Foo
--  Qux :: String -> Foo

--data Expr a where
--  IntVal :: Int -> Expr Int
--  BoolVal :: Bool -> Expr Bool
--  Plus :: Expr Int -> Expr Int -> Expr Int
--  And :: Expr Bool -> Expr Bool -> Expr Bool
--
--x :: Expr Int
--x = Plus (BoolVal True) (BoolVal False)

-- %d %s
data FormatType a where
  IntType :: FormatType Int
  StringType :: FormatType String

data Format a where
  FNil :: Format String
  (:%) :: FormatType a -> Format b -> Format (a -> b)
  (:>) :: String -> Format a -> Format a

infixr 9 :%

infixr 9 :>

--formatter :: Format (Int -> String -> String)
--formatter = IntType :% StringType :% FNil

printfFormat :: Format a -> a
printfFormat = go []
  where
    go :: [String] -> Format a -> a
    go acc FNil = concat $ reverse acc
    go acc (s :> for) = go (s : acc) for
    go acc (ft :% for) =
      \x ->
        case ft of
          IntType -> go (show x : acc) for
          StringType -> go (x : acc) for

-- DataKinds

--type family F a :: Type where
--  F Bool = Int
--  F Char = String
--  F _ = ()
--
--x :: F Bool
--x = 5

-- (5, 3) :: (Int, Int)
-- '(Int, String)
type family Fst tup :: a where
  Fst '(x, _) = x

type family Snd tup :: a where
  Snd '(_, x) = x

data FormatArg
  = Lit Symbol
  | IntArg
  | StringArg

-- Parse "pesho has %d apples"
-- ->
-- '[Lit "pesho has ", IntArg, Lit " apples"]

-- ToList
-- symbols след 9.2 не е нужна
type family Parse sym :: [FormatArg] where
  Parse str = ToFormatArgs (ToList str)

type family SpanFormat (syms :: [Symbol]) :: (Symbol, [Symbol]) where
  SpanFormat '[] = '("", '[])
  SpanFormat ("%" ': c ': str) = '("", "%" ': c ': str)
  SpanFormat (x ': xs) =
    '( AppendSymbol x (Fst (SpanFormat xs)),
       Snd (SpanFormat xs)
     )

-- 'a' : 'b' : 'c' : []
-- "abc"
type family ToFormatArgs (syms :: [Symbol]) :: [FormatArg] where
  ToFormatArgs '[] = '[]
  ToFormatArgs ("%" ': "d" ': str) = IntArg ': ToFormatArgs str
  ToFormatArgs ("%" ': "s" ': str) = StringArg ': ToFormatArgs str
  ToFormatArgs str =
    Lit (Fst (SpanFormat str)) ': ToFormatArgs (Snd (SpanFormat str))

type family ResultType xs :: Type where
  ResultType '[] = String
  ResultType (IntArg ': xs) = Int -> ResultType xs
  ResultType (StringArg ': xs) = String -> ResultType xs
  ResultType (Lit _ ': xs) = ResultType xs

--printf :: ResultType (Parse sym)
--printf = undefined

-- printf @"pesho has %d apples" :: Int -> String
-- искам sym -> "pesho has %d apples"
--printf :: ToFunType sym
--printf = undefined

-- FormatType - *Ty
-- Format - :%, :>, infixr
-- printfFormat

-- FormatArg - *Arg
-- Parse
-- SpanFormat
-- ToFormatArgs (TypeError, Text, ShowType :<>: ??)
-- FormatArgsToFormat
-- printf
