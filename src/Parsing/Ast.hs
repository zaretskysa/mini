module Parsing.Ast where

import Types

data Program = Program [SourceElement]
    deriving (Show, Eq)

data SourceElement = 
      StatementSourceElement Statement
    | FunctionDeclSourceElement FunctionDeclaration
    deriving (Show, Eq)

data FunctionDeclaration = 
    FunctionDeclaration Identifier Identifiers [SourceElement]
    deriving (Show, Eq)

type MaybeStatement = Maybe Statement

data Statement = 
      EmptyStatement
    | BlockStatement [Statement]
    | ExpressionStatement AssignmentExpression
    | VarDeclStatement String AdditiveExpression
    | IfStatement AdditiveExpression Statement MaybeStatement
    | ReturnStatement MaybeLogicalAndExpression
    deriving (Show, Eq)

data AssignmentExpression = 
      LogicalAndAssignmentExpression LogicalAndExpression
    | AssignmentOperatorExpression String LogicalAndExpression
    deriving (Show, Eq)

type MaybeAdditiveExpression = Maybe AdditiveExpression

data LogicalAndExpression =
      UnaryLogicalAndExpression AdditiveExpression
    | BinaryLogicalAndExpression LogicalAndExpression AdditiveExpression
    deriving (Show, Eq)

type MaybeLogicalAndExpression = Maybe LogicalAndExpression

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
