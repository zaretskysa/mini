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
      UnaryMultExpression Double
    | MultMultExpression MultExpression Double
    | DivMultExpression MultExpression Double
    deriving (Show, Eq)
