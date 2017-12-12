module TypeCheck where

import Control.Monad (void, unless)
import SemanticMonad
import TypeError
import AbsLatte

-- TODO XXX check for void and function types
--


returns :: PStmt -> Bool
returns (Ret _ _) = True
returns (VRet _) = True
returns (CondElse _ _ ifStmt elseStmt) = returns ifStmt && returns elseStmt
returns (BStmt _ (Block _ stmts)) = any returns stmts
returns _ = False


typeCheck :: PProgram -> Either TypeError SymTable
typeCheck prog = runTypeCheck $ checkProg prog


checkProg :: PProgram -> TCheck ()
checkProg (Program _ fdefs) = do
    mapM_ addFType fdefs
    mapM_ checkFDef fdefs


addFType :: PTopDef -> TCheck ()
addFType (FnDef pos retType fname args _) = do
    let argTypes = map (\(Arg _ t _) -> t) args
    declare (Fun pos retType argTypes) fname pos


checkFDef :: PTopDef -> TCheck ()
checkFDef (FnDef pos retType fname args body) = do
    -- TODO push block
    mapM_ (\(Arg pos t i) -> declare t i pos) args
    --declare retType (Ident "$ret") pos -- TODO XXX
    enterFunction retType
    checkBlock body
    -- XXX
    unless (rmpos retType == pVoid || (returns $ BStmt nopos body))
           (raise $ noReturn fname pos)
    --modify $ Map.delete $ Ident "$ret"
    leaveFunction
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
    retType <- returnType
    void $ matchTypes retType exprType pos

checkStmt (VRet pos) = do
    retType <- returnType
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
    unless (elem t1' [pInt, pStr]) (raise $ typeMismatch t1 pInt pos) -- XXX
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
    matchTypes ftype (Fun nopos ret argTypes) pos
    return ret
