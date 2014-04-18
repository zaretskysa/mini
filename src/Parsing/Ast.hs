module Parsing.Ast where

import Types

data Program = Program [SourceElement]
    deriving (Show, Eq)

data SourceElement = 
      StatementSourceElement Statement
    | FunctionDeclSourceElement Identifier Identifiers FunctionBody
    deriving (Show, Eq)

data FunctionBody = 
    FunctionBody [SourceElement]
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
      UnaryAdditiveExpression MultExpression
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
    | IdentAccessExpression Identifier
    | CallAccessExpression Identifier [AssignmentExpression]
    deriving (Show, Eq)
