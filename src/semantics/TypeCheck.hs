{-# LANGUAGE FlexibleInstances #-}

module TypeCheck where

import Control.Monad (when, void)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import AbsLatte

-- TODO XXX check for void and function types
--


data TypeError = TypeMismatch PType PType
               | UndeclaredVariable String
               | MultipleDeclarations  -- TODO
               | NoReturn
    deriving (Show, Eq)


class Monad m => MonadTypeCheck m where
    matchTypes :: Type a -> Type b -> m PType
    typeof :: Ident -> m PType
    declare :: PType -> Ident -> m ()
    runTypeCheck :: m a -> Either TypeError a

type TCheck = ExceptT TypeError (State Env)

instance MonadTypeCheck TCheck where
    matchTypes t1 t2 = do
        let t1' = (nopos <$ t1) :: PType
        let t2' = (nopos <$ t2) :: PType
        when (t1' /= t2') (throwError $ TypeMismatch t1' t2')
        return t1'

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
addFType (FnDef ni retType fname args _) = do
    let argTypes = Prelude.map (\(Arg _ t _) -> t) args
    declare (Fun ni retType argTypes) fname


checkFDef :: PTopDef -> TCheck ()
checkFDef (FnDef _ retType fname args body) = do
    -- TODO push block
    mapM_ (\(Arg _ t i) -> declare t i) args
    declare retType $ Ident "$ret" -- TODO XXX
    checkBlock body
    unless (returns $ BStmt nopos body) (throwError NoReturn)
    modify $ Map.delete $ Ident "$ret"
    -- TODO pop block


checkBlock :: PBlock -> TCheck ()
checkBlock (Block _ stmts) = do
    -- TODO push block
    mapM_ checkStmt stmts
    -- TODO pop block


checkStmt :: PStmt -> TCheck ()
checkStmt (Empty _) = return ()

checkStmt (Decl _ typ items) = mapM_ foo items where
    foo (NoInit _ ident) = declare typ ident
    foo (Init _ ident expr) = do
        exprType <- checkExpr expr
        matchTypes exprType typ
        declare typ ident

checkStmt (Ass _ ident expr) = do
    exprType <- checkExpr expr
    varType <- typeof ident
    void $ matchTypes varType exprType

checkStmt (Incr _ ident) = do
    varType <- typeof ident
    void $ matchTypes varType (Int nopos)

checkStmt (Decr ni ident) = checkStmt (Incr ni ident) -- XXX?

checkStmt (Ret _ expr) = do
    exprType <- checkExpr expr
    retType <- typeof $ Ident "$ret"  -- XXX
    void $ matchTypes retType exprType

checkStmt (VRet _) = do
    retType <- typeof $ Ident "$ret"  -- XXX
    void $ matchTypes retType (Void nopos)

checkStmt (Cond _ expr stmt) = do
    exprType <- checkExpr expr
    matchTypes exprType (Bool nopos)
    checkStmt stmt

checkStmt (CondElse ni expr ifStmt elseStmt) = do
    checkStmt $ Cond ni expr ifStmt  -- XXX
    checkStmt elseStmt

checkStmt (While ni expr stmt) = do
    checkStmt $ Cond ni expr stmt  -- XX

checkStmt (SExp _ expr) = do
    void $ checkExpr expr


checkExpr :: PExpr -> TCheck PType
checkExpr (EVar _ ident) = typeof ident

checkExpr (ELitTrue ni) = return $ Bool ni

checkExpr (ELitFalse ni) = return $ Bool ni

checkExpr (ELitInt ni _) = return $ Int ni

checkExpr (EString ni _) = return $ Str ni

checkExpr (EAdd _ expr1 (Plus _) expr2) = do
    t1 <- checkExpr expr1
    t2 <- checkExpr expr2
    matchTypes t1 t2
    let t1' = rmpos t1
    unless (elem t1' [(Int ()), (Str ())]) (throwError $ TypeMismatch t1 (Int nopos)) -- XXX
    return t1

checkExpr (EAdd _ expr1 (Minus ni) expr2) = do
    t1 <- checkExpr expr1
    t2 <- checkExpr expr2
    matchTypes t1 t2
    matchTypes t1 $ Int nopos

checkExpr (ERel _ expr1 op expr2) = do
    t1 <- checkExpr expr1
    t2 <- checkExpr expr2
    matchTypes t1 t2

checkExpr (EMul _ expr1 op expr2) = do
    t1 <- checkExpr expr1
    t2 <- checkExpr expr2
    matchTypes t1 t2
    matchTypes t1 $ Int nopos

checkExpr (Not _ expr) = do
    typ <- checkExpr expr
    matchTypes typ $ Bool nopos

checkExpr (Neg _ expr) = do
    typ <- checkExpr expr
    matchTypes typ $ Int nopos

checkExpr (EAnd _ expr1 expr2) = do
    t1 <- checkExpr expr1
    t2 <- checkExpr expr2
    matchTypes t1 t2  -- TODO what can be &&-ed?

checkExpr (EOr ni expr1 expr2) = checkExpr (EAnd ni expr1 expr2) -- XXX

checkExpr (EApp _ fident args) = do
    ftype@(Fun ni ret _) <- typeof fident
    argTypes <- mapM checkExpr args
    matchTypes ftype (Fun ni ret argTypes)
    return ret
