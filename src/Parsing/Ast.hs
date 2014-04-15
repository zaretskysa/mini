module Parsing.Ast where

data Program = Program [Statement]
    deriving (Show, Eq)

data Statement = 
      ExpressionStatement AssignmentExpression
    | VarDeclStatement String AdditiveExpression
    deriving (Show, Eq)

data AssignmentExpression = 
      AdditiveAssignmentExpression AdditiveExpression
    | AssignmentOperatorExpression String AdditiveExpression
    deriving (Show, Eq)

data AdditiveExpression =
      UnaryExpression MultExpression
    | PlusExpression AdditiveExpression MultExpression
    | MinusExpression AdditiveExpression MultExpression
    deriving (Show, Eq)

data MultExpression =
      UnaryMultExpression AccessExpression
    | MultMultExpression MultExpression AccessExpression
    | DivMultExpression MultExpression AccessExpression
    deriving (Show, Eq)

data AccessExpression = 
      DoubleAccessExpression Double
    | IdentAccessExpression String
    deriving (Show, Eq)
