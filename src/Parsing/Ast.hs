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
    | IfStatement LogicalOrExpression Statement MaybeStatement
    | ReturnStatement MaybeLogicalOrExpression
    | ThrowStatement LogicalOrExpression
    | TryCatchStatement Block Catch
    deriving (Show, Eq)

type Block = [Statement]

data Catch = Catch Identifier Block deriving (Show, Eq)

data AssignmentExpression = 
      LogicalOrAssignmentExpression LogicalOrExpression
    | AssignmentOperatorExpression String LogicalOrExpression
    deriving (Show, Eq)

data LogicalOrExpression =
      UnaryLogicalOrExpression LogicalAndExpression
    | BinaryLogicalOrExpression LogicalOrExpression LogicalAndExpression
    deriving (Show, Eq)

type MaybeLogicalOrExpression = Maybe LogicalOrExpression

data LogicalAndExpression =
      UnaryLogicalAndExpression EqualityExpression
    | BinaryLogicalAndExpression LogicalAndExpression EqualityExpression
    deriving (Show, Eq)

type MaybeLogicalAndExpression = Maybe LogicalAndExpression

data EqualityExpression = 
      UnaryEqualityExpression AdditiveExpression
    | EqualsExpression EqualityExpression AdditiveExpression
    | NotEqualsExpression EqualityExpression AdditiveExpression
    deriving (Show, Eq)

data AdditiveExpression =
      UnaryAdditiveExpression MultExpression
    | PlusExpression AdditiveExpression MultExpression
    | MinusExpression AdditiveExpression MultExpression
    deriving (Show, Eq)

type MaybeAdditiveExpression = Maybe AdditiveExpression

data MultExpression =
      UnaryMultExpression AccessExpression
    | MultMultExpression MultExpression AccessExpression
    | DivMultExpression MultExpression AccessExpression
    deriving (Show, Eq)

data AccessExpression = 
      DoubleAccessExpression Double
    | BoolAccessExpression Bool
    | StringAccessExpression String
    | IdentAccessExpression Identifier
    | CallAccessExpression Identifier [AssignmentExpression]
    deriving (Show, Eq)
