{-# LANGUAGE FlexibleInstances #-}

module TypeCheck where

import Control.Monad (when, void)
import Control.Monad.Except
import Control.Monad.State
import Data.Map as Map
import AbsLatte

-- TODO XXX check for void and function types
--

data TypeError = TypeMismatch Type Type
               | UndeclaredVariable String
               | MultipleDeclarations  -- TODO
               | NoReturn
    deriving (Show, Eq)


class Monad m => MonadTypeCheck m where
    matchTypes :: Type -> Type -> m Type
    typeof :: Ident -> m Type
    declare :: Type -> Ident -> m ()
    runTypeCheck :: m a -> Either TypeError a

type TCheck = ExceptT TypeError (State Env)

instance MonadTypeCheck TCheck where
    matchTypes t1 t2 = do
        when (t1 /= t2) (throwError $ TypeMismatch t1 t2)
        return t1

    typeof var@(Ident name) = do
        maybeType <- gets $ Map.lookup var
        case maybeType of
            Nothing -> throwError $ UndeclaredVariable name
            Just t -> return t

    declare typ var = do
        env <- get
        when (Map.member var env) (throwError MultipleDeclarations)
        modify $ Map.insert var typ

    runTypeCheck tc = evalState (runExceptT tc) Map.empty


type Env = Map.Map Ident Type


returns :: Stmt -> Bool
returns (Ret _) = True
returns VRet = True
returns (CondElse _ ifStmt elseStmt) = returns ifStmt && returns elseStmt
returns (BStmt (Block stmts)) = any returns stmts
returns _ = False


typeCheck :: Program -> TCheck ()
typeCheck (Program fdefs) = do
    mapM_ addFType fdefs
    mapM_ checkFDef fdefs


addFType :: TopDef -> TCheck ()
addFType (FnDef retType fname args _) = do
    let argTypes = Prelude.map (\(Arg t _) -> t) args
    declare (Fun retType argTypes) fname


checkFDef :: TopDef -> TCheck ()
checkFDef (FnDef retType fname args body) = do
    -- TODO push block
    mapM_ (\(Arg t i) -> declare t i) args
    declare retType $ Ident "$ret" -- TODO XXX
    checkBlock body
    unless (returns $ BStmt body) (throwError NoReturn)
    modify $ Map.delete $ Ident "$ret"
    -- TODO pop block


checkBlock :: Block -> TCheck ()
checkBlock (Block stmts) = do
    -- TODO push block
    mapM_ checkStmt stmts
    -- TODO pop block


checkStmt :: Stmt -> TCheck ()
checkStmt Empty = return ()

checkStmt (Decl typ items) = mapM_ foo items where
    foo (NoInit ident) = declare typ ident
    foo (Init ident expr) = do
        exprType <- checkExpr expr
        matchTypes exprType typ
        declare typ ident

checkStmt (Ass ident expr) = do
    exprType <- checkExpr expr
    varType <- typeof ident
    void $ matchTypes varType exprType

checkStmt (Incr ident) = do
    varType <- typeof ident
    void $ matchTypes varType Int

checkStmt (Decr ident) = checkStmt (Incr ident) -- XXX?

checkStmt (Ret expr) = do
    exprType <- checkExpr expr
    retType <- typeof $ Ident "$ret"  -- XXX
    void $ matchTypes retType exprType

checkStmt VRet = do
    retType <- typeof $ Ident "$ret"  -- XXX
    void $ matchTypes retType Void

checkStmt (Cond expr stmt) = do
    exprType <- checkExpr expr
    matchTypes exprType Bool
    checkStmt stmt

checkStmt (CondElse expr ifStmt elseStmt) = do
    checkStmt $ Cond expr ifStmt  -- XXX
    checkStmt elseStmt

checkStmt (While expr stmt) = do
    checkStmt $ Cond expr stmt  -- XX

checkStmt (SExp expr) = do
    void $ checkExpr expr


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

checkExpr (EApp fident args) = do
    ftype@(Fun ret _) <- typeof fident
    argTypes <- mapM checkExpr args
    matchTypes ftype (Fun ret argTypes)
    return ret
