{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad.Identity
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Error

type MyState = String

data Statement =
      Return Value
    | GetState
    | SetState MyState
    | FuncCall [Statement]
    | Throw Value
    | TryCatch [Statement] [Statement]
    deriving (Show)

data Value = 
      Undefined
    | Value Int 
    | StateValue MyState
    deriving (Show, Eq)

data Exception = Exception Value deriving (Show, Eq)

instance Error Exception where
    noMsg = Exception Undefined
    strMsg str = Exception Undefined


type Eval a = ContT Value (ErrorT Exception (StateT MyState Identity)) a

instance MonadError e m => MonadError e (ContT r m) where
  throwError = lift . throwError
  catchError op h = ContT $ \k -> catchError (runContT op k) (\e -> runContT (h e) k)


runEval ::(Eval Value) -> MyState -> (Either Exception Value, MyState)
runEval eval state = runIdentity (runStateT (runErrorT (runContT eval return)) state)

evalProg :: [Statement] -> Either Exception Value
evalProg stmts = fst $ runEval (evalBlock stmts) $ ""

evalBlock :: [Statement] -> Eval Value
evalBlock [] = return Undefined
evalBlock [stmt] = evalStatment stmt
evalBlock (st:stmts) = evalStatment st >> evalBlock stmts

evalStatment :: Statement -> Eval Value
evalStatment (Return val) = funcReturn val
evalStatment (SetState state) = put state >> return Undefined
evalStatment (FuncCall stmts) = funcCall stmts
evalStatment GetState = get >>= return . StateValue
evalStatment (Throw val) = throwError $ Exception val
evalStatment (TryCatch tryBlock catchBlock) =
    let handler = \_e -> evalBlock catchBlock
    in (evalBlock tryBlock) `catchError` handler


funcCall :: [Statement] -> Eval Value
funcCall stmts = lift $ runContT (evalBlock stmts) return

funcReturn :: Value -> Eval Value
funcReturn val = ContT $ \_ -> return val


test0 = evalProg [FuncCall [Return Undefined]] -- result is Value 1
test1 = evalProg [FuncCall [Return Undefined], Return Undefined] -- result is Value 2
test2 = evalProg [SetState "Hello", FuncCall [SetState "Galaxy", Return Undefined], GetState] -- result is StateValue "Galaxy"
test3 = evalProg [FuncCall [Throw Undefined, Return Undefined]]
