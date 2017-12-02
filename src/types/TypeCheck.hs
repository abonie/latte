{-# LANGUAGE FlexibleInstances #-}

module TypeCheck where

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Map as Map
import AbsLatte

-- TODO XXX check for void and function types
--

data TypeError = TypeMismatch Type Type | UndeclaredVariable String deriving (Show, Eq)


class Monad m => MonadTypeCheck m where
    matchTypes :: Type -> Type -> m Type
    typeof :: Ident -> m Type
    runTypeCheck :: m a -> Either TypeError a

type TCheck = ExceptT TypeError (Reader Env)

instance MonadTypeCheck TCheck where
    matchTypes t1 t2 = do
        when (t1 /= t2) (throwError $ TypeMismatch t1 t2)
        return t1

    typeof var@(Ident name) = do
        maybeType <- asks $ Map.lookup var
        case maybeType of
            Nothing -> throwError $ UndeclaredVariable name
            Just t -> return t

    runTypeCheck tc = runReader (runExceptT tc) Map.empty


type Env = Map.Map Ident Type

typeCheck :: Program -> TCheck ()
typeCheck (Program fdefs) = do
    env <- funcTypes fdefs  -- TODO rename refactor
    local (const env) (mapM_ checkFDef fdefs)


funcTypes :: [TopDef] -> TCheck Env
funcTypes [] = ask

funcTypes ((FnDef retType fname args body):defs) = do
    env <- ask
    let argTypes = Prelude.map (\(Arg t _) -> t) args
    local (Map.insert fname $ Fun retType argTypes) (funcTypes defs)


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
    matchTypes exprType typ
    local (Map.insert ident typ) (checkStmt $ Decl typ rest)

checkStmt (Ass ident expr) = do
    exprType <- checkExpr expr
    varType <- typeof ident
    matchTypes varType exprType
    ask

checkStmt (Incr ident) = do
    varType <- asks $ flip (!) $ ident
    matchTypes varType Int
    ask

checkStmt (Decr ident) = checkStmt (Incr ident) -- XXX?

checkStmt (Ret expr) = ask -- TODO

checkStmt VRet = ask -- TODO

checkStmt (Cond expr stmt) = do
    exprType <- checkExpr expr
    matchTypes exprType Bool
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
checkExpr (EVar ident) = typeof ident

checkExpr (ELitTrue) = return Bool

checkExpr (ELitFalse) = return Bool

checkExpr (ELitInt _) = return Int

checkExpr (EString _) = return Str

checkExpr (EAdd expr1 Plus expr2) = do
    t1 <- checkExpr expr1
    t2 <- checkExpr expr2
    matchTypes t1 t2
    unless (elem t1 [Int, Str]) (throwError $ TypeMismatch t1 Int) -- XXX
    return t1

checkExpr (EAdd expr1 Minus expr2) = do
    t1 <- checkExpr expr1
    t2 <- checkExpr expr2
    matchTypes t1 t2
    matchTypes t1 Int

checkExpr (ERel expr1 op expr2) = do
    t1 <- checkExpr expr1
    t2 <- checkExpr expr2
    matchTypes t1 t2

checkExpr (EMul expr1 op expr2) = do
    t1 <- checkExpr expr1
    t2 <- checkExpr expr2
    matchTypes t1 t2
    matchTypes t1 Int

checkExpr (Not expr) = do
    typ <- checkExpr expr
    matchTypes typ Bool

checkExpr (Neg expr) = do
    typ <- checkExpr expr
    matchTypes typ Int

checkExpr (EAnd expr1 expr2) = do
    t1 <- checkExpr expr1
    t2 <- checkExpr expr2
    matchTypes t1 t2  -- TODO what can be &&-ed?

checkExpr (EOr expr1 expr2) = checkExpr (EAnd expr1 expr2) -- XXX

--checkExpr (EApp fident args) = do
--    -- TODO
