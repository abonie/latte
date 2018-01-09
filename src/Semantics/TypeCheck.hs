{-# LANGUAGE LambdaCase #-}

module Semantics.TypeCheck where
import qualified Data.Map as Map
import Control.Monad (void, unless)
import Parsing.AbsLatte
import Semantics.SemanticMonad
import Semantics.TypeError
import Errors.LatteError


typeCheck :: PProgram -> Either (LatteError PType) (Program (PosInfo, TypeInfo))
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
                               (Block nopos [])
    ]


checkProg :: PProgram -> TCheck (Program (PosInfo, TypeInfo))
checkProg (Program pos defs) = do
    mapM_ addTypeDecl builtins
    mapM_ addTypeDecl defs
    defs' <- mapM checkTopDef defs
    return $ Program (pos, novars) defs'


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
    return $ FnDef (pos, novars)
        (FunDef (posf, novars) (mapnovars retType) fname (map mapnovars args) (Block (posb, novars) stmts'))

checkTopDef clsdef@(ClsDef pos name ext body) = do
    return $ mapnovars clsdef


checkBlock :: PBlock -> TCheck (Block (PosInfo, TypeInfo))
checkBlock (Block pos stmts) = do
    enterBlock
    stmts' <- mapM checkStmt stmts
    leaveBlock
    return $ Block (pos, novars) stmts'


checkItem :: PType -> PItem -> TCheck (Item (PosInfo, TypeInfo))
checkItem typ (NoInit pos ident) = do
    declare typ ident pos
    return $ NoInit (pos, (Just typ, Map.empty)) ident

checkItem typ (Init pos ident expr) = do
    (exprType, expr') <- checkExpr expr
    matchTypes exprType typ pos
    declare typ ident pos
    return $ Init (pos, (Just typ, Map.empty)) ident expr'


checkStmt :: PStmt -> TCheck (Stmt (PosInfo, TypeInfo))
checkStmt (Empty pos) = return $ Empty (pos, novars)

checkStmt (BStmt pos block) = do
    block' <- checkBlock block
    return $ BStmt (pos, novars) block'

checkStmt (Decl pos typ items) = do
    items' <- mapM (checkItem typ) items
    return $ Decl (pos, (Just typ, Map.empty)) (settype typ typ) items'

checkStmt (Ass pos (LhsVar posv ident) expr) = do
    (exprType, expr') <- checkExpr expr
    varType <- typeof ident pos
    void $ matchTypes varType exprType pos
    let varinfo = (Nothing, Map.singleton ident exprType)
    return $ Ass (pos, varinfo) (LhsVar (posv, varinfo) ident) expr'

checkStmt (Incr pos (LhsVar posv ident)) = do
    varType <- typeof ident pos
    void $ matchTypes varType pInt pos
    let varinfo = (Nothing, Map.singleton ident varType)
    return $ Incr (pos, varinfo) (LhsVar (posv, varinfo) ident)

-- XXX boiler
checkStmt (Decr pos (LhsVar posv ident)) = do
    varType <- typeof ident pos
    void $ matchTypes varType pInt pos
    let varinfo = (Nothing, Map.singleton ident varType)
    return $ Decr (pos, varinfo) (LhsVar (posv, varinfo) ident)

checkStmt (Ret pos expr) = do
    (exprType, expr') <- checkExpr expr
    retType <- returnType
    void $ matchTypes retType exprType pos
    return $ Ret (pos, novars) expr'

checkStmt (VRet pos) = do
    retType <- returnType
    void $ matchTypes retType pVoid pos
    return $ VRet (pos, novars)

checkStmt (Cond pos expr stmt) = do
    (exprType, expr') <- checkExpr expr
    matchTypes exprType pBool pos
    stmt' <- checkStmt stmt
    return $ Cond (pos, novars) expr' stmt'

-- XXX boiler
checkStmt (CondElse pos expr ifStmt elseStmt) = do
    (exprType, expr') <- checkExpr expr
    matchTypes exprType pBool pos
    ifStmt' <- checkStmt ifStmt
    elseStmt' <- checkStmt elseStmt
    return $ CondElse (pos, novars) expr' ifStmt' elseStmt'

checkStmt (While pos expr stmt) = do
    (Cond info' expr' stmt') <- checkStmt $ Cond pos expr stmt  -- XXX
    return $ While info' expr' stmt'

checkStmt (SExp pos expr) = do
    (_, expr') <- checkExpr expr
    return $ SExp (pos, novars) expr'


checkExpr :: PExpr -> TCheck (PType, Expr (PosInfo, TypeInfo))
checkExpr (EVar pos ident) = do
    varType <- typeof ident pos
    let varinfo = (Just varType, Map.singleton ident varType)
    return (varType, EVar (pos, varinfo) ident)

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
    return (t1, EAdd (pos, (typeInfo t1)) expr1' (Plus (poso, novars)) expr2')

checkExpr (EAdd pos expr1 (Minus poso) expr2) = do
    (t1, expr1') <- checkExpr expr1
    (t2, expr2') <- checkExpr expr2
    matchTypes t1 t2 pos
    matchTypes t1 pInt pos
    return (pInt, EAdd (pos, (typeInfo pInt)) expr1' (Minus (poso, novars)) expr2')

checkExpr (ERel pos expr1 op expr2) = do
    (t1, expr1') <- checkExpr expr1
    (t2, expr2') <- checkExpr expr2
    matchTypes t1 t2 pos
    return (Bool pos, ERel (pos, (typeInfo pBool)) expr1' (mapnovars op) expr2')

checkExpr (EMul pos expr1 op expr2) = do
    (t1, expr1') <- checkExpr expr1
    (t2, expr2') <- checkExpr expr2
    matchTypes t1 t2 pos
    matchTypes t1 pInt pos
    return (pInt, EMul (pos, (typeInfo pInt)) expr1' (mapnovars op) expr2')

checkExpr (Not pos expr) = do
    (typ, expr') <- checkExpr expr
    matchTypes typ pBool pos
    return (pBool, Not (pos, (typeInfo pBool)) expr')

checkExpr (Neg pos expr) = do
    (typ, expr') <- checkExpr expr
    matchTypes typ pInt pos
    return (pInt, Neg (pos, (typeInfo pInt)) expr')

checkExpr (EAnd pos expr1 expr2) = do
    (t1, expr1') <- checkExpr expr1
    (t2, expr2') <- checkExpr expr2
    matchTypes t1 t2 pos -- TODO what can be &&-ed?
    return (t2, EAnd (pos, (typeInfo t2)) expr1' expr2')

-- XXX boiler
checkExpr (EOr pos expr1 expr2) = do
    (t1, expr1') <- checkExpr expr1
    (t2, expr2') <- checkExpr expr2
    matchTypes t1 t2 pos -- See above?
    return (t2, EOr (pos, (typeInfo t2)) expr1' expr2')

checkExpr (EApp pos fident args) = do
    ftype@(Fun _ ret _) <- typeof fident pos
    argsChecked <- mapM checkExpr args
    let args' = map snd argsChecked
    let argTypes = map fst argsChecked
    matchTypes ftype (Fun nopos ret argTypes) pos
    return (ret, EApp (pos, typeInfo ret) fident args')


-- utils

returns :: PStmt -> Bool
returns (Ret _ _) = True
returns (VRet _) = True
returns (CondElse _ _ ifStmt elseStmt) = returns ifStmt && returns elseStmt
returns (BStmt _ (Block _ stmts)) = any returns stmts
returns _ = False


type TypeInfo = (Maybe PType, Map.Map Ident PType)

novars :: TypeInfo
novars = (Nothing, Map.empty)

typeInfo :: PType -> TypeInfo
typeInfo t = (Just t, Map.empty)

-- XXX rename
settype :: Functor f => PType -> f a -> f (a, TypeInfo)
settype t = fmap $ flip (,) (Just t, Map.empty)

mapnovars :: Functor f => f a -> f (a, TypeInfo)
mapnovars = fmap $ flip (,) novars
