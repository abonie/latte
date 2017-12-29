{-# LANGUAGE LambdaCase #-}

module Semantics.TypeCheck where

import Control.Monad (void, unless)
import Parsing.AbsLatte
import Semantics.SemanticMonad
import Semantics.TypeError
import Errors.LatteError


returns :: PStmt -> Bool
returns (Ret _ _) = True
returns (VRet _) = True
returns (CondElse _ _ ifStmt elseStmt) = returns ifStmt && returns elseStmt
returns (BStmt _ (Block _ stmts)) = any returns stmts
returns _ = False


typeCheck :: PProgram -> Either (LatteError PType) SymTable
typeCheck prog = runTypeCheck $ checkProg prog


checkProg :: PProgram -> TCheck ()
checkProg (Program _ defs) = do
    -- XXX ugly
    mapM_ addTypeDecl $ filter (\case { FnDef _ _ -> False; _ -> True }) defs
    mapM_ addTypeDecl $ filter (\case { FnDef _ _ -> True; _ -> False }) defs
    mapM_ checkTopDef defs


addTypeDecl :: PTopDef -> TCheck ()
addTypeDecl (FnDef pos (FunDef _ retType fname args _)) = do
    let argTypes = map (\(Arg _ t _) -> t) args
    declare (Fun pos retType argTypes) fname pos

addTypeDecl (ClsDef pos name ext body) = do
    addClass name ((\case { ExtNone _ -> Nothing; ExtSome _ x -> Just x }) ext) pos

checkTopDef :: PTopDef -> TCheck ()
checkTopDef (FnDef pos (FunDef _ retType fname args body@(Block _ stmts))) = do
    enterFunction retType
    mapM_ (\(Arg pos t i) -> declare t i pos) args  -- XXX
    mapM_ checkStmt stmts  -- XXX
    -- XXX
    unless (rmpos retType == pVoid || (returns $ BStmt nopos body))
           (raise $ noReturn fname pos)
    leaveFunction

checkTopDef (ClsDef _ name ext body) = do
    return ()  -- TODO

checkBlock :: PBlock -> TCheck ()
checkBlock (Block _ stmts) = do
    enterBlock
    mapM_ checkStmt stmts
    leaveBlock


checkStmt :: PStmt -> TCheck ()
checkStmt (Empty _) = return ()

checkStmt (BStmt _ block) = checkBlock block

checkStmt (Decl _ typ items) = mapM_ foo items where
    foo (NoInit pos ident) = declare typ ident pos
    foo (Init pos ident expr) = do
        exprType <- checkExpr expr
        matchTypes exprType typ pos
        declare typ ident pos

checkStmt (Ass pos (LhsVar _ ident) expr) = do
    exprType <- checkExpr expr
    varType <- typeof ident pos
    void $ matchTypes varType exprType pos

checkStmt (Incr pos (LhsVar _ ident)) = do
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

checkStmt (While pos expr stmt) = checkStmt $ Cond pos expr stmt  -- XXX

checkStmt (SExp _ expr) = void $ checkExpr expr


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
