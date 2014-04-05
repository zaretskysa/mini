module Parsing.Ast where

data Program = Program [Statement]
    deriving (Show, Eq)

data Statement = ExpressionStatement Expression
    deriving (Show, Eq)

data Expression =
      UnaryExpression MultExpression
    | PlusExpression Expression MultExpression
    | MinusExpression Expression MultExpression
    deriving (Show, Eq)

data MultExpression =
      UnaryMultExpression Integer
    | MultMultExpression MultExpression Integer
    | DivMultExpression MultExpression Integer
    deriving (Show, Eq)
