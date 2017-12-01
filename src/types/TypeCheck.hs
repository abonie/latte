module TypeCheck where

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Map as Map
import AbsLatte


data TypeError = TypeMismatch Type Type deriving (Show, Eq)

type TCheck a = ExceptT TypeError (Reader Env) a

type Env = Map.Map Ident Type

typeCheck :: Program -> TCheck ()
typeCheck (Program fdefs) = do
    mapM_ checkFDef fdefs


checkFDef :: TopDef -> TCheck ()
checkFDef (FnDef retType fname args body) = do
    local (Map.union $ Map.fromList $ Prelude.map (\(Arg t i) -> (i, t)) args) (checkBlock body)
    

checkBlock :: Block -> TCheck ()
checkBlock (Block (st:tl)) = do
    env <- checkStmt st
    local (const env) (checkBlock (Block tl))

checkBlock (Block []) = return ()


checkStmt :: Stmt -> TCheck Env
checkStmt Empty = ask

checkStmt (Decl _ []) = ask
checkStmt (Decl typ ((NoInit ident):rest)) = do
    local (Map.insert ident typ) (checkStmt (Decl typ rest))
checkStmt (Decl typ ((Init ident expr):rest)) = do
    exprType <- checkExpr expr
    when (exprType /= typ) (throwError $ TypeMismatch typ exprType)
    local (Map.insert ident typ) (checkStmt $ Decl typ rest)

checkStmt (Ass ident expr) = do
    exprType <- checkExpr expr
    varType <- asks $ flip (!) $ ident  -- TODO
    when (varType /= exprType) (throwError $ TypeMismatch varType exprType)
    ask

checkStmt (Incr ident) = do
    varType <- asks $ flip (!) $ ident
    when (varType /= Int) (throwError $ TypeMismatch varType Int)
    ask
    
checkStmt (Decr ident) = checkStmt (Incr ident) -- XXX?

checkStmt (Ret expr) = ask -- TODO

checkStmt VRet = ask -- TODO

checkStmt (Cond expr stmt) = do
    exprType <- checkExpr expr
    when (exprType /= Bool) (throwError $ TypeMismatch exprType Bool)
    checkStmt stmt
    ask

checkStmt (CondElse expr ifStmt elseStmt) = do
    checkStmt $ Cond expr ifStmt  -- XXX
    checkStmt elseStmt
    ask

checkStmt (While expr stmt) = do
    checkStmt $ Cond expr stmt  -- XX
    ask

checkStmt (SExp expr) = do
    checkExpr expr
    ask


checkExpr :: Expr -> TCheck Type
checkExpr (EVar ident) = asks $ flip (!) $ ident  -- TODO

checkExpr (ELitTrue) = return Bool

checkExpr (ELitFalse) = return Bool

checkExpr (ELitInt _) = return Int

checkExpr (EString _) = return Str
