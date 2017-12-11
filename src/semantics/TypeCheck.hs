{-# LANGUAGE FlexibleInstances #-}

module TypeCheck where

import Control.Monad (when, void)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import AbsLatte

-- TODO XXX check for void and function types
--


data TypeError = TypeMismatch PType PType PosInfo
               | UndeclaredVariable String PosInfo
               | MultipleDeclarations PosInfo
               | NoReturn PosInfo
    deriving (Show, Eq)


class Monad m => MonadTypeCheck m where
    matchTypes :: PType -> PType -> PosInfo -> m PType
    typeof :: Ident -> PosInfo -> m PType
    declare :: PType -> Ident -> PosInfo -> m ()
    runTypeCheck :: m a -> Either TypeError a

type TCheck = ExceptT TypeError (State Env)

instance MonadTypeCheck TCheck where
    matchTypes t1 t2 pos = do
        let t1' = rmpos t1
        let t2' = rmpos t2
        when (t1' /= t2') (throwError $ TypeMismatch t1 t2 pos)
        return t1

    typeof var@(Ident name) pos = do
        maybeType <- gets $ Map.lookup var
        case maybeType of
            Nothing -> throwError $ UndeclaredVariable name pos
            Just t -> return t

    declare typ var pos = do
        env <- get
        when (Map.member var env) (throwError $ MultipleDeclarations pos)
        modify $ Map.insert var typ

    runTypeCheck tc = evalState (runExceptT tc) Map.empty


type Env = Map.Map Ident PType


returns :: PStmt -> Bool
returns (Ret _ _) = True
returns (VRet _) = True
returns (CondElse _ _ ifStmt elseStmt) = returns ifStmt && returns elseStmt
returns (BStmt _ (Block _ stmts)) = any returns stmts
returns _ = False


typeCheck :: PProgram -> TCheck ()
typeCheck (Program _ fdefs) = do
    mapM_ addFType fdefs
    mapM_ checkFDef fdefs


addFType :: PTopDef -> TCheck ()
addFType (FnDef pos retType fname args _) = do
    let argTypes = Prelude.map (\(Arg _ t _) -> t) args
    declare (Fun pos retType argTypes) fname pos


checkFDef :: PTopDef -> TCheck ()
checkFDef (FnDef pos retType fname args body) = do
    -- TODO push block
    mapM_ (\(Arg pos t i) -> declare t i pos) args
    declare retType (Ident "$ret") pos -- TODO XXX
    checkBlock body
    -- XXX
    unless (rmpos retType == pVoid || (returns $ BStmt nopos body))
           (throwError $ NoReturn pos)
    modify $ Map.delete $ Ident "$ret"
    -- TODO pop block


checkBlock :: PBlock -> TCheck ()
checkBlock (Block _ stmts) = do
    -- TODO push block
    mapM_ checkStmt stmts
    -- TODO pop block


checkStmt :: PStmt -> TCheck ()
checkStmt (Empty _) = return ()

checkStmt (BStmt _ block) = checkBlock block

checkStmt (Decl _ typ items) = mapM_ foo items where
    foo (NoInit pos ident) = declare typ ident pos
    foo (Init pos ident expr) = do
        exprType <- checkExpr expr
        matchTypes exprType typ pos
        declare typ ident pos

checkStmt (Ass pos ident expr) = do
    exprType <- checkExpr expr
    varType <- typeof ident pos
    void $ matchTypes varType exprType pos

checkStmt (Incr pos ident) = do
    varType <- typeof ident pos
    void $ matchTypes varType pInt pos

checkStmt (Decr pos ident) = checkStmt (Incr pos ident) -- XXX?

checkStmt (Ret pos expr) = do
    exprType <- checkExpr expr
    retType <- typeof (Ident "$ret") pos  -- XXX
    void $ matchTypes retType exprType pos

checkStmt (VRet pos) = do
    retType <- typeof (Ident "$ret") pos  -- XXX
    void $ matchTypes retType pVoid pos

checkStmt (Cond pos expr stmt) = do
    exprType <- checkExpr expr
    matchTypes exprType pBool pos
    checkStmt stmt

checkStmt (CondElse pos expr ifStmt elseStmt) = do
    checkStmt $ Cond pos expr ifStmt  -- XXX
    checkStmt elseStmt

checkStmt (While pos expr stmt) = do
    checkStmt $ Cond pos expr stmt  -- XX

checkStmt (SExp _ expr) = do
    void $ checkExpr expr


checkExpr :: PExpr -> TCheck PType
checkExpr (EVar pos ident) = typeof ident pos

checkExpr (ELitTrue pos) = return $ Bool pos

checkExpr (ELitFalse pos) = return $ Bool pos

checkExpr (ELitInt pos _) = return $ Int pos

checkExpr (EString pos _) = return $ Str pos

checkExpr (EAdd pos expr1 (Plus _) expr2) = do
    t1 <- checkExpr expr1
    t2 <- checkExpr expr2
    matchTypes t1 t2 pos
    let t1' = rmpos t1
    unless (elem t1' [pInt, pStr]) (throwError $ TypeMismatch t1 pInt pos) -- XXX
    return t1

checkExpr (EAdd pos expr1 (Minus _) expr2) = do
    t1 <- checkExpr expr1
    t2 <- checkExpr expr2
    matchTypes t1 t2 pos
    matchTypes t1 pInt pos

checkExpr (ERel pos expr1 op expr2) = do
    t1 <- checkExpr expr1
    t2 <- checkExpr expr2
    matchTypes t1 t2 pos
    return $ Bool pos

checkExpr (EMul pos expr1 op expr2) = do
    t1 <- checkExpr expr1
    t2 <- checkExpr expr2
    matchTypes t1 t2 pos
    matchTypes t1 pInt pos

checkExpr (Not pos expr) = do
    typ <- checkExpr expr
    matchTypes typ pBool pos

checkExpr (Neg pos expr) = do
    typ <- checkExpr expr
    matchTypes typ pInt pos

checkExpr (EAnd pos expr1 expr2) = do
    t1 <- checkExpr expr1
    t2 <- checkExpr expr2
    matchTypes t1 t2 pos -- TODO what can be &&-ed?

checkExpr (EOr pos expr1 expr2) = checkExpr (EAnd pos expr1 expr2) -- XXX

checkExpr (EApp pos fident args) = do
    ftype@(Fun _ ret _) <- typeof fident pos
    argTypes <- mapM checkExpr args
    matchTypes ftype (Fun pos ret argTypes) pos
    return ret
