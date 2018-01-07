{-# LANGUAGE LambdaCase #-}

module Compile.CodeGen where
import qualified LLVM
import Control.Monad (void)
import Data.Maybe (fromJust)
import Parsing.AbsLatte
import Errors.LatteError
import Semantics.TypeCheck (TypeInfo (..))
import Compile.Monad


compile :: Program (PosInfo, TypeInfo) -> Either (LatteError PType) LLVM.Module
compile prog = runGen $ genProg prog


genProg :: Program (PosInfo, TypeInfo) -> LLGen ()
genProg (Program _ defs) = do
    mapM_ genTopDef defs


genTopDef :: TopDef (PosInfo, TypeInfo) -> LLGen ()
genTopDef (FnDef _ (FunDef _ retType fname args body@(Block _ stmts))) = do
    beginFunction retType fname args
    mapM_ genStmt stmts
    endFunction


genBlock :: Block (PosInfo, TypeInfo) -> LLGen ()
genBlock (Block _ stmts) = do
    beginScope
    mapM_ genStmt stmts
    endScope

genStmt :: Stmt (PosInfo, TypeInfo) -> LLGen ()
genStmt (Empty _) = return ()

genStmt (BStmt _ block) = genBlock block

genStmt (Decl _ typ items) = mapM_
    (\case {
        NoInit info ident -> addVar typ ident (fst info);
        Init info ident expr -> do {
            addVar typ ident (fst info);
            genStmt $ Ass info (LhsVar info ident) expr } } ) -- XXX
    items

genStmt (Ass _ (LhsVar _ ident) expr) = do
    x <- genExpr expr
    setVar ident x

genStmt (Incr _ (LhsVar _ ident)) = do
    r <- newReg
    val <- getVar ident
    emit $ LLVM.Bin r LLVM.Add LLVM.I32 val (LLVM.LitInt 1)
    setVar ident (LLVM.Reg r)

genStmt (Decr _ (LhsVar _ ident)) = do
    r <- newReg
    val <- getVar ident
    emit $ LLVM.Bin r LLVM.Sub LLVM.I32 val (LLVM.LitInt 1)
    setVar ident (LLVM.Reg r)

genStmt (Ret _ expr) = do
    x <- genExpr expr
    emit $ LLVM.Ret LLVM.I32 x  -- TODO XXX type

genStmt (VRet _) = do
    emit $ LLVM.VRet

genStmt (Cond _ expr stmt) = do
    x <- genExpr expr
    lold <- currentLabel
    ltrue <- newLabel
    lfalse <- newLabel
    emit $ LLVM.Cbr x (LLVM.Reg ltrue) (LLVM.Reg lfalse)
    emit $ LLVM.Label ltrue
    startPhi
    genStmt stmt
    emit $ LLVM.Br (LLVM.Reg lfalse)
    emit $ LLVM.Label lfalse
    endPhi lold ltrue

genStmt (CondElse _ expr trueStmt falseStmt) = do
    x <- genExpr expr
    ltrue <- newLabel
    lfalse <- newLabel
    lafter <- newLabel
    emit $ LLVM.Cbr x (LLVM.Reg ltrue) (LLVM.Reg lfalse)
    emit $ LLVM.Label ltrue
    genStmt trueStmt
    emit $ LLVM.Br (LLVM.Reg lafter)
    emit $ LLVM.Label lfalse
    genStmt falseStmt
    emit $ LLVM.Br (LLVM.Reg lafter)
    emit $ LLVM.Label lafter

genStmt (While _ expr stmt) = do
    lcond <- newLabel
    lloop <- newLabel
    lafter <- newLabel
    emit $ LLVM.Label lcond
    x <- genExpr expr
    emit $ LLVM.Cbr x (LLVM.Reg lloop) (LLVM.Reg lafter)
    emit $ LLVM.Label lloop
    genStmt stmt
    emit $ LLVM.Br (LLVM.Reg lcond)
    emit $ LLVM.Label lafter

genStmt (SExp _ expr) = void $ genExpr expr


genExpr :: Expr (PosInfo, TypeInfo) -> LLGen LLVM.Operand
genExpr (EVar _ ident) = getVar ident

genExpr (ELitTrue _) = return $ LLVM.LitInt 1  -- XXX

genExpr (ELitFalse _) = return $ LLVM.LitInt 0

genExpr (ELitInt _ n) = return $ LLVM.LitInt n

genExpr (EString _ s) = do
    r <- newReg
    glob <- addStr s
    let len = (length s) - 1
    emit $ LLVM.Bitcast r (LLVM.Ptr (LLVM.Array len LLVM.I8)) glob (LLVM.Ptr LLVM.I8)
    return (LLVM.Reg r)

genExpr (EAdd _ expr1 op expr2) = genBinop (addOp2LLVM op) expr1 expr2  -- TODO XXX check for strings

genExpr (EMul _ expr1 op expr2) = genBinop (mulOp2LLVM op) expr1 expr2

genExpr (ERel _ expr1 op expr2) = do
    r <- newReg
    x1 <- genExpr expr1
    x2 <- genExpr expr2
    emit $ LLVM.Cmp r (cmpOp2LLVM op) LLVM.I1 x1 x2
    return (LLVM.Reg r)

genExpr (Not _ expr) = do
    x <- genExpr expr
    r <- newReg
    emit $ LLVM.Bin r LLVM.Sub LLVM.I1 (LLVM.LitInt 1) x
    return (LLVM.Reg r)

genExpr (Neg _ expr) = do
    x <- genExpr expr
    r <- newReg
    emit $ LLVM.Bin r LLVM.Mul LLVM.I32 x $ LLVM.LitInt (-1)
    return (LLVM.Reg r)

genExpr (EAnd _ expr1 expr2) = do
    l1 <- currentLabel
    x1 <- genExpr expr1
    r1 <- newReg
    l2 <- newLabel
    l3 <- newLabel
    emit $ LLVM.Cmp r1 LLVM.Eq LLVM.I1 x1 (LLVM.LitInt 0)
    emit $ LLVM.Cbr (LLVM.Reg r1) (LLVM.Reg l3) (LLVM.Reg l2)
    emit $ LLVM.Label l2
    x2 <- genExpr expr2
    emit $ LLVM.Br (LLVM.Reg l3)
    emit $ LLVM.Label l3
    r3 <- newReg
    emit $ LLVM.Phi r3 LLVM.I1 (LLVM.LitInt 0) l1 x2 l2
    return (LLVM.Reg r3)

genExpr (EOr _ expr1 expr2) = do
    l1 <- currentLabel
    x1 <- genExpr expr1
    r1 <- newReg
    l2 <- newLabel
    l3 <- newLabel
    emit $ LLVM.Cmp r1 LLVM.Eq LLVM.I1 x1 (LLVM.LitInt 1)
    emit $ LLVM.Cbr (LLVM.Reg r1) (LLVM.Reg l3) (LLVM.Reg l2)
    emit $ LLVM.Label l2
    x2 <- genExpr expr2
    emit $ LLVM.Br (LLVM.Reg l3)
    emit $ LLVM.Label l3
    r3 <- newReg
    emit $ LLVM.Phi r3 LLVM.I1 (LLVM.LitInt 1) l1 x2 l2
    return (LLVM.Reg r3)

genExpr (EApp (_, (Just typ, _)) (Ident fname) args) = do
    r <- newReg
    argValues <- mapM genExpr args
    let argTypes = map (typeToLLVM . typeOfExpr) args
    let cargs = map (uncurry LLVM.Carg) $ zip argTypes argValues
    -- XXX TODO type
    emit $ LLVM.Call r (typeToLLVM typ) (LLVM.Ident '@':fname) cargs
    return (LLVM.Reg r)


genBinop :: LLVM.Binop -> Expr (PosInfo, TypeInfo) -> Expr (PosInfo, TypeInfo) -> LLGen LLVM.Operand
genBinop op expr1 expr2 = do
    t1 <- genExpr expr1
    t2 <- genExpr expr2
    r <- newReg
    emit $ LLVM.Bin r op LLVM.I32 t1 t2
    return (LLVM.Reg r)


-- TODO XXX
cmpOp2LLVM :: RelOp a -> LLVM.Cmpop
cmpOp2LLVM (LTH a) = LLVM.Lt
cmpOp2LLVM (LE a) = LLVM.Le
cmpOp2LLVM (GTH a) = LLVM.Gt
cmpOp2LLVM (GE a) = LLVM.Ge
cmpOp2LLVM (EQU a) = LLVM.Eq
cmpOp2LLVM (NE a) = LLVM.Ne

addOp2LLVM :: AddOp a -> LLVM.Binop
addOp2LLVM (Plus _) = LLVM.Add
addOp2LLVM (Minus _) = LLVM.Sub

mulOp2LLVM :: MulOp a -> LLVM.Binop
mulOp2LLVM (Times _) = LLVM.Mul
mulOp2LLVM (Div _) = LLVM.Div

typeOfExpr :: Expr (PosInfo, TypeInfo) -> PType
typeOfExpr (EVar (_, (Just t, _)) _) = t
typeOfExpr (EMem (_, (Just t, _)) _ _) = t
typeOfExpr (EInd (_, (Just t, _)) _ _) = t
typeOfExpr (ELitInt _ _) = pInt
typeOfExpr (ELitTrue _) = pBool
typeOfExpr (ELitFalse _) = pBool
typeOfExpr (ENew (_, (Just t, _)) _) = t
typeOfExpr (ENull (_, (Just t, _)) _) = t
typeOfExpr (EArr (_, (Just t, _)) _ _) = t
typeOfExpr (EApp (_, (Just t, _)) _ _) = t
typeOfExpr (EMet (_, (Just t, _)) _ _ _) = t
typeOfExpr (EString _ _) = pStr
typeOfExpr (Neg _ _) = pInt
typeOfExpr (Not _ _ ) = pBool
typeOfExpr (EMul _ _ _ _) = pInt
typeOfExpr (EAdd (_, (Just t, _)) _ _ _) = t
typeOfExpr (ERel _ _ _ _) = pBool
typeOfExpr (EAnd _ _ _ ) = pBool
typeOfExpr (EOr _ _ _) = pBool
