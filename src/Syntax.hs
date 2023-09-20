module Syntax where

import Prelude hiding (Integral)

type Name = String
type Position = Word
type FileName = Name
type Location = (Position, Position, FileName)

class Localizable a where
  start    :: a -> Position
  end      :: a -> Position
  fileName :: a -> FileName

data File = File FileName Term Location deriving (Eq, Show)

instance Localizable File where
  start (File _ _ (s,_,_))    = s
  end (File _ _ (_,e,_))      = e
  fileName (File _ _ (_,_,f)) = f

data BinaryOperator
  = Add
  | Sub
  | Mul
  | Div
  | Rem
  | Eq
  | Neq
  | Lt
  | Gt
  | Lte
  | Gte
  | And
  | Or
  deriving (Eq, Show)

newtype Integral a = Integral a deriving (Eq, Ord, Show)

data Term
  = Error String String Location
  | Int (Integral Int) Location
  | Str (Integral String) Location
  | Bool (Integral Bool) Location
  | Call Term [Term] Location
  | Binary Term BinaryOperator Term Location
  | Function
  | Let
  | If { condition :: Term, thenTerm :: Term, otherwiseTerm :: Term }
  | Print
  | First
  | Second
  | Tuple
  | Var Name Location
  deriving (Eq, Show)
