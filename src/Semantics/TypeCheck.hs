{-# LANGUAGE LambdaCase #-}

module Semantics.TypeCheck where
import qualified Data.Map as Map
import Control.Monad (void, unless)
import Parsing.AbsLatte
import Semantics.SemanticMonad
import Semantics.TypeError
import Semantics.TypeInfo
import Errors.LatteError


typeCheck :: PProgram -> Either (LatteError PType) (Program (PosInfo, TypeInfo), TypeEnv)
typeCheck prog = runTypeCheck $ checkProg prog


builtins :: [TopDef PosInfo]
builtins = [
    FnDef nopos $ FunDef nopos (pVoid)
                               (Ident "printInt")
                               [Arg nopos pInt (Ident "x")]
                               (Block nopos []),
    FnDef nopos $ FunDef nopos (pVoid)
                               (Ident "printString")
                               [Arg nopos pStr (Ident "s")]
                               (Block nopos []),
    FnDef nopos $ FunDef nopos (pInt)
                               (Ident "readInt")
                               []
                               (Block nopos []),
    FnDef nopos $ FunDef nopos (pStr)
                               (Ident "readString")
                               []
                               (Block nopos []),
    FnDef nopos $ FunDef nopos (pVoid)
                               (Ident "error")
                               []
                               (Block nopos [])
    ]


checkProg :: PProgram -> TCheck (Program (PosInfo, TypeInfo))
checkProg (Program pos defs) = do
    mapM_ addTypeDecl builtins
    mapM_ addTypeDecl defs
    -- TODO right now class declaration must be before its usage in source code
    defs' <- mapM checkTopDef defs
    return $ Program (pos, Nothing) defs'


addTypeDecl :: PTopDef -> TCheck ()
addTypeDecl (FnDef pos (FunDef _ retType fname args _)) = do
    let argTypes = map (\(Arg _ t _) -> t) args
    declare (Fun pos retType argTypes) fname pos

addTypeDecl (ClsDef pos name ext body) = do
    addClass name ((\case { ExtNone _ -> Nothing; ExtSome _ x -> Just x }) ext) pos

checkTopDef :: PTopDef -> TCheck (TopDef (PosInfo, TypeInfo))
checkTopDef (FnDef pos (FunDef posf retType fname args body@(Block posb stmts))) = do
    enterFunction retType
    mapM_ (\(Arg pos t i) -> declare t i pos) args  -- XXX
    stmts' <- mapM checkStmt stmts  -- XXX
    -- XXX
    unless (rmpos retType == pVoid || (returns $ BStmt nopos body))
           (raise $ noReturn fname pos)
    leaveFunction
    return $ FnDef (pos, Nothing)
        (FunDef (posf, Nothing) (mapnovars retType) fname (map mapnovars args) (Block (posb, Nothing) stmts'))

checkTopDef (ClsDef pos name ext body@(CBody posb decls)) = do
    enterClass name
    decls' <- mapM checkMem decls
    leaveClass
    return $ ClsDef (pos, Nothing) name (mapnovars ext) $ CBody (posb, Nothing) decls'


checkMem :: MemDecl PosInfo -> TCheck (MemDecl (PosInfo, TypeInfo))
checkMem (MemVar pos typ idents) = do
    mapM_ (\ident -> declare typ ident pos) idents
    return $ MemVar (pos, Just typ) (mapnovars typ) idents


checkBlock :: PBlock -> TCheck (Block (PosInfo, TypeInfo))
checkBlock (Block pos stmts) = do
    enterBlock
    stmts' <- mapM checkStmt stmts
    leaveBlock
    return $ Block (pos, Nothing) stmts'


checkItem :: PType -> PItem -> TCheck (Item (PosInfo, TypeInfo))
checkItem typ (NoInit pos ident) = do
    declare typ ident pos
    return $ NoInit (pos, Just typ) ident

checkItem typ (Init pos ident expr) = do
    (exprType, expr') <- checkExpr expr
    matchTypes exprType typ pos
    declare typ ident pos
    return $ Init (pos, Just typ) ident expr'


checkStmt :: PStmt -> TCheck (Stmt (PosInfo, TypeInfo))
checkStmt (Empty pos) = return $ Empty (pos, Nothing)

checkStmt (BStmt pos block) = do
    block' <- checkBlock block
    return $ BStmt (pos, Nothing) block'

checkStmt (Decl pos typ items) = do
    items' <- mapM (checkItem typ) items
    return $ Decl (pos, Just typ) (settype typ typ) items'

checkStmt (Ass pos (LhsVar posv ident) expr) = do
    (exprType, expr') <- checkExpr expr
    varType <- typeof ident pos
    void $ matchTypes varType exprType pos
    return $ Ass (pos, Nothing) (LhsVar (posv, Just varType) ident) expr'

checkStmt (Ass pos (LhsInd posi ident indExpr) expr) = do
    (indType, indExpr') <- checkExpr indExpr
    matchTypes indType pInt posi
    -- TODO XXX check bounds ?
    (exprType, expr') <- checkExpr expr
    varType <- typeof ident pos
    matchTypes varType (Arr nopos exprType) pos
    return $ Ass (pos, Nothing) (LhsInd (posi, Just exprType) ident indExpr') expr'

checkStmt (Ass pos (LhsMem posm obj mem) expr) = do
    (exprType, expr') <- checkExpr expr
    clsType <- typeof obj pos
    case clsType of
        TCls _ clsName -> do
            t <- memberType clsName mem pos
            matchTypes t exprType pos
            return $ Ass (pos, Nothing) (LhsMem (posm, Just t) obj mem) expr'
        _ -> raise $ otherError (Just "expected an object") pos

checkStmt (Incr pos (LhsVar posv ident)) = do
    varType <- typeof ident pos
    void $ matchTypes varType pInt pos
    return $ Incr (pos, Nothing) (LhsVar (posv, Just varType) ident)

checkStmt (Incr pos (LhsInd posi ident expr)) = do
    (exprType, expr') <- checkExpr expr
    matchTypes exprType pInt posi
    varType <- typeof ident pos
    matchTypes varType (Arr nopos pInt) pos
    return $ Incr (pos, Nothing) (LhsInd (posi, Just varType) ident expr')

-- XXX boiler
checkStmt (Decr pos (LhsVar posv ident)) = do
    varType <- typeof ident pos
    void $ matchTypes varType pInt pos
    return $ Decr (pos, Nothing) (LhsVar (posv, Just varType) ident)

checkStmt (Decr pos (LhsInd posi ident expr)) = do
    (exprType, expr') <- checkExpr expr
    matchTypes exprType pInt posi
    varType <- typeof ident pos
    matchTypes varType (Arr nopos pInt) pos
    return $ Decr (pos, Nothing) (LhsInd (posi, Just varType) ident expr')

checkStmt (Ret pos expr) = do
    (exprType, expr') <- checkExpr expr
    retType <- returnType
    void $ matchTypes retType exprType pos
    return $ Ret (pos, Nothing) expr'

checkStmt (VRet pos) = do
    retType <- returnType
    void $ matchTypes retType pVoid pos
    return $ VRet (pos, Nothing)

checkStmt (Cond pos expr stmt) = do
    (exprType, expr') <- checkExpr expr
    matchTypes exprType pBool pos
    stmt' <- checkStmt stmt
    -- XXX
    case expr of
        ELitTrue _ -> return $ stmt'
        -- ELitFalse _ -> return $ Empty (nopos, Nothing)
        _ -> return $ Cond (pos, Nothing) expr' stmt'

-- XXX boiler
checkStmt (CondElse pos expr ifStmt elseStmt) = do
    (exprType, expr') <- checkExpr expr
    matchTypes exprType pBool pos
    ifStmt' <- checkStmt ifStmt
    elseStmt' <- checkStmt elseStmt
    -- XXX
    case expr of
        ELitTrue _ -> return $ ifStmt'
        ELitFalse _ -> return $ elseStmt'
        _ -> return $ CondElse (pos, Nothing) expr' ifStmt' elseStmt'

checkStmt (While pos expr stmt) = do
    (exprType, expr') <- checkExpr expr
    matchTypes exprType pBool pos
    stmt' <- checkStmt stmt
    return $ While (pos, Nothing) expr' stmt'

checkStmt (For pos typ ident expr stmt) = do
    (exprType, expr') <- checkExpr expr
    matchTypes exprType (Arr nopos typ) pos
    enterBlock
    declare typ ident pos
    stmt' <- checkStmt stmt
    leaveBlock
    return $ For (pos, Nothing) (mapnovars typ) ident expr' stmt'
    
checkStmt (SExp pos expr) = do
    (_, expr') <- checkExpr expr
    return $ SExp (pos, Nothing) expr'


checkExpr :: PExpr -> TCheck (PType, Expr (PosInfo, TypeInfo))
checkExpr (EVar pos ident) = do
    varType <- typeof ident pos
    return (varType, EVar (pos, Just varType) ident)

checkExpr (EMem pos ident mem) = do
    varType <- typeof ident pos
    -- TODO XXX boilerplate - check if type is an array of something
    case (varType, mem) of
        (Arr _ typ, Ident "length") -> return (pInt, EMem (pos, Just pInt) ident mem)
        (TCls _ cls, _) -> do
            memType <- memberType cls mem pos
            return (memType, EMem (pos, Just memType) ident mem)
        _ -> raise $ otherError (Just "expected array type") pos  -- XXX

checkExpr (EInd pos ident expr) = do
    (exprType, expr') <- checkExpr expr
    matchTypes exprType pInt pos
    varType <- typeof ident pos
    -- TODO XXX Check if type matches (Arr _ something) !!!
    case varType of
        Arr _ typ -> return (typ, EInd (pos, Just typ) ident expr')
        _ -> raise $ otherError (Just "expected array type") pos  -- XXX

checkExpr (ENew pos ident) = do
    checkClass ident pos
    let typ = TCls nopos ident
    return (typ, ENew (pos, Just typ) ident)

checkExpr (ENull pos ident) = do
    checkClass ident pos
    let typ = TCls nopos ident
    return (typ, ENull (pos, Just typ) ident)

checkExpr lit@(ELitTrue pos) = return (Bool pos, settype (Bool pos) lit)

checkExpr lit@(ELitFalse pos) = return (Bool pos, settype (Bool pos) lit)

checkExpr lit@(ELitInt pos _) = return (Int pos, settype (Int pos) lit)

checkExpr lit@(EString pos _) = return (Str pos, settype (Str pos) lit)

checkExpr (EAdd pos expr1 (Plus poso) expr2) = do
    (t1, expr1') <- checkExpr expr1
    (t2, expr2') <- checkExpr expr2
    matchTypes t1 t2 pos
    let t1' = rmpos t1
    unless (elem t1' [pInt, pStr]) (raise $ typeMismatch t1 pInt pos) -- XXX
    return (t1, EAdd (pos, Just t1) expr1' (Plus (poso, Nothing)) expr2')

checkExpr (EAdd pos expr1 (Minus poso) expr2) = do
    (t1, expr1') <- checkExpr expr1
    (t2, expr2') <- checkExpr expr2
    matchTypes t1 t2 pos
    matchTypes t1 pInt pos
    return (pInt, EAdd (pos, Just pInt) expr1' (Minus (poso, Nothing)) expr2')

checkExpr (ERel pos expr1 op expr2) = do
    (t1, expr1') <- checkExpr expr1
    (t2, expr2') <- checkExpr expr2
    matchTypes t1 t2 pos
    return (pBool, ERel (pos, Just pBool) expr1' (mapnovars op) expr2')

checkExpr (EMul pos expr1 op expr2) = do
    (t1, expr1') <- checkExpr expr1
    (t2, expr2') <- checkExpr expr2
    matchTypes t1 t2 pos
    matchTypes t1 pInt pos
    return (pInt, EMul (pos, Just pInt) expr1' (mapnovars op) expr2')

checkExpr (Not pos expr) = do
    (typ, expr') <- checkExpr expr
    matchTypes typ pBool pos
    return (pBool, Not (pos, Just pBool) expr')

checkExpr (Neg pos expr) = do
    (typ, expr') <- checkExpr expr
    matchTypes typ pInt pos
    return (pInt, Neg (pos, Just pInt) expr')

checkExpr (EAnd pos expr1 expr2) = do
    (t1, expr1') <- checkExpr expr1
    (t2, expr2') <- checkExpr expr2
    matchTypes t1 t2 pos -- TODO what can be &&-ed?
    return (t2, EAnd (pos, Just t2) expr1' expr2')

-- XXX boiler
checkExpr (EOr pos expr1 expr2) = do
    (t1, expr1') <- checkExpr expr1
    (t2, expr2') <- checkExpr expr2
    matchTypes t1 t2 pos -- See above?
    return (t2, EOr (pos, Just t2) expr1' expr2')

checkExpr (EArr pos typ expr) = do
    (typExpr, expr') <- checkExpr expr
    matchTypes typExpr typ pos
    return (Arr nopos typExpr, EArr (pos, Just typExpr) (mapnovars typ) expr')

checkExpr (EApp pos fident args) = do
    ftype@(Fun _ ret _) <- typeof fident pos
    argsChecked <- mapM checkExpr args
    let args' = map snd argsChecked
    let argTypes = map fst argsChecked
    matchTypes ftype (Fun nopos ret argTypes) pos
    return (ret, EApp (pos, Just ret) fident args')


-- utils
-- TODO move to separate file

returns :: Stmt a -> Bool
returns (Ret _ _) = True
returns (VRet _) = True
returns (CondElse _ cond ifStmt elseStmt) = case cond of
    ELitTrue _ -> returns ifStmt
    ELitFalse _ -> returns elseStmt
    _ -> returns ifStmt && returns elseStmt
returns (BStmt _ (Block _ stmts)) = any returns stmts
returns (Cond _ (ELitTrue _) stmt) = returns stmt
returns _ = False
