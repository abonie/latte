{-# LANGUAGE LambdaCase #-}

module Compile.CodeGen where
import qualified LLVM
import qualified Data.Map as Map
import Control.Monad (void, forM_, when, unless)
import Parsing.AbsLatte
import Errors.LatteError
import Semantics.TypeInfo
import Semantics.TypeCheck (returns) -- TODO move to different module and maybe rename
import Compile.Monad


compile :: Program (PosInfo, TypeInfo) -> Either (LatteError PType) LLVM.Module
compile prog = runGen $ genProg prog


globDecls :: [LLVM.TopDef]
globDecls = [
    LLVM.FunDec LLVM.Void
                (LLVM.Ident "@printInt")
                [LLVM.Arg LLVM.I32 (LLVM.Ident "%x")],
    LLVM.FunDec LLVM.Void
                (LLVM.Ident "@printString")
                [LLVM.Arg (LLVM.Ptr LLVM.I8) (LLVM.Ident "%s")],
    LLVM.FunDec LLVM.I32
                (LLVM.Ident "@readInt")
                [],
    LLVM.FunDec (LLVM.Ptr LLVM.I8)
                (LLVM.Ident "@readString")
                [],
    LLVM.FunDec LLVM.Void
                (LLVM.Ident "@llvm.memcpy.p0i8.p0i8.i64")
                [LLVM.Arg (LLVM.Ptr LLVM.I8) (LLVM.Ident "%dst"),
                 LLVM.Arg (LLVM.Ptr LLVM.I8) (LLVM.Ident "%src"),
                 LLVM.Arg LLVM.I64 (LLVM.Ident "%len"),
                 LLVM.Arg LLVM.I32 (LLVM.Ident "%algn"),
                 LLVM.Arg LLVM.I1 (LLVM.Ident "%volatile")],
    LLVM.FunDec LLVM.I64
                (LLVM.Ident "@strlen")
                [LLVM.Arg (LLVM.Ptr LLVM.I8) (LLVM.Ident "%str")],
    LLVM.FunDec (LLVM.Ptr LLVM.I8)
                (LLVM.Ident "@malloc")
                [LLVM.Arg LLVM.I64 (LLVM.Ident "%n")]
    ]
    
genProg :: Program (PosInfo, TypeInfo) -> LLGen ()
genProg (Program _ defs) = do
    mapM_ addDecl globDecls
    mapM_ genTopDef defs


genTopDef :: TopDef (PosInfo, TypeInfo) -> LLGen ()
genTopDef (FnDef _ (FunDef _ retType fname args body@(Block _ stmts))) = do
    beginFunction retType fname args
    mapM_ genStmt stmts
    endFunction


genBlock :: Block (PosInfo, TypeInfo) -> LLGen ()
genBlock (Block _ stmts) = do
    newScope
    mapM_ genStmt stmts
    endScope

genStmt :: Stmt (PosInfo, TypeInfo) -> LLGen ()
genStmt (Empty _) = return ()

genStmt (BStmt _ block) = genBlock block

genStmt (Decl _ typ items) = forM_ items (\case
        NoInit info ident -> do
            addVar typ ident (fst info)
            case typ of
                -- TODO
                Str _ -> genStmt $ Ass info (LhsVar info ident) (EString (nopos, Nothing) "")
                Int _ -> genStmt $ Ass info (LhsVar info ident) (ELitInt (nopos, Nothing) 0)
                Bool _ -> genStmt $ Ass info (LhsVar info ident) (ELitFalse (nopos, Nothing))
                _ -> return ()
        Init info ident expr -> do
            x <- genExpr expr
            addVar typ ident (fst info)
            setVar ident x )

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
    let t = typeToLLVM $ typeOf expr
    emit $ LLVM.Ret t x

genStmt (VRet _) = do
    emit $ LLVM.VRet

-- TODO create explicit representation for basic blocks
genStmt (Cond _ expr stmt) = do
    x <- genExpr expr
    ltrue <- newLabel
    lfalse <- newLabel
    emit $ LLVM.Cbr x (LLVM.Reg ltrue) (LLVM.Reg lfalse)
    setLabel ltrue
    genStmt stmt
    unless (returns stmt) (emit $ LLVM.Br (LLVM.Reg lfalse))
    setLabel lfalse

genStmt (CondElse _ expr tStmt fStmt) = do
    x <- genExpr expr
    ltrue <- newLabel
    lfalse <- newLabel
    lafter <- newLabel
    emit $ LLVM.Cbr x (LLVM.Reg ltrue) (LLVM.Reg lfalse)
    setLabel ltrue
    genStmt tStmt
    unless (returns tStmt) (emit $ LLVM.Br (LLVM.Reg lafter))
    setLabel lfalse
    genStmt fStmt
    unless (returns fStmt) (emit $ LLVM.Br (LLVM.Reg lafter))
    setLabel lafter

genStmt (While _ expr stmt) = do
    lcond <- newLabel
    lloop <- newLabel
    lafter <- newLabel
    emit $ LLVM.Br (LLVM.Reg lcond)
    setLabel lcond
    x <- genExpr expr
    emit $ LLVM.Cbr x (LLVM.Reg lloop) (LLVM.Reg lafter)
    setLabel lloop
    genStmt stmt
    emit $ LLVM.Br (LLVM.Reg lcond)
    setLabel lafter

genStmt (SExp _ expr) = void $ genExpr expr


genExpr :: Expr (PosInfo, TypeInfo) -> LLGen LLVM.Operand
genExpr (EVar _ ident) = do
    getVar ident

genExpr (ELitTrue _) = return $ LLVM.LitInt 1

genExpr (ELitFalse _) = return $ LLVM.LitInt 0

genExpr (ELitInt _ n) = return $ LLVM.LitInt n

genExpr (EString _ s) = do
    r <- newReg
    glob <- addStr s r
    let len = max 1 $ (length s) - 1
    emit $ LLVM.Bitcast r (LLVM.Ptr (LLVM.Array len LLVM.I8)) glob (LLVM.Ptr LLVM.I8)
    return (LLVM.Reg r)

genExpr (EAdd (_, Just typ) expr1 op expr2) = case typ of
    Int _ -> genBinop (addOp2LLVM op) expr1 expr2
    Str _ -> do
        -- TODO check if operator is Add ? type checking ensures that
        x1 <- genExpr expr1
        x2 <- genExpr expr2
        l1Reg <- callStrlen x1
        l2Reg <- callStrlen x2
        len <- newReg
        sum <- newReg
        emit $ LLVM.Bin sum LLVM.Add LLVM.I64 l1Reg l2Reg
        emit $ LLVM.Bin len LLVM.Add LLVM.I64 (LLVM.Reg sum) (LLVM.LitInt 1)
        res <- newReg
        emit $ LLVM.Call res (LLVM.Ptr LLVM.I8) (LLVM.Ident "@malloc") [LLVM.Carg LLVM.I64 (LLVM.Reg len)]
        let args1 = [
                LLVM.Carg (LLVM.Ptr LLVM.I8) (LLVM.Reg res),
                LLVM.Carg (LLVM.Ptr LLVM.I8) x1,
                LLVM.Carg LLVM.I64 l1Reg,
                LLVM.Carg LLVM.I32 (LLVM.LitInt 0),
                LLVM.Carg LLVM.I1 (LLVM.LitInt 1)
                ]
        emit $ LLVM.VCall LLVM.Void (LLVM.Ident "@llvm.memcpy.p0i8.p0i8.i64") args1
        ptr <- newReg
        emit $ LLVM.GEP ptr LLVM.I8 (LLVM.Reg res) l1Reg
        l2plus1 <- newReg
        emit $ LLVM.Bin l2plus1 LLVM.Add LLVM.I64 l2Reg (LLVM.LitInt 1)
        let args2 = [
                LLVM.Carg (LLVM.Ptr LLVM.I8) (LLVM.Reg ptr),
                LLVM.Carg (LLVM.Ptr LLVM.I8) x2,
                LLVM.Carg LLVM.I64 (LLVM.Reg l2plus1),
                LLVM.Carg LLVM.I32 (LLVM.LitInt 0),
                LLVM.Carg LLVM.I1 (LLVM.LitInt 1)
                    ]
        emit $ LLVM.VCall LLVM.Void (LLVM.Ident "@llvm.memcpy.p0i8.p0i8.i64") args2
        return $ LLVM.Reg res

genExpr (EMul _ expr1 op expr2) = genBinop (mulOp2LLVM op) expr1 expr2

genExpr (ERel _ expr1 op expr2) = do
    r <- newReg
    x1 <- genExpr expr1
    x2 <- genExpr expr2
    -- XXX
    let typ = typeToLLVM $ typeOf expr1
    emit $ LLVM.Cmp r (cmpOp2LLVM op) typ x1 x2
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
    x1 <- genExpr expr1
    l1 <- currentLabel
    l2 <- newLabel
    r1 <- newReg
    l3 <- newLabel
    emit $ LLVM.Cmp r1 LLVM.Eq LLVM.I1 x1 (LLVM.LitInt 0)
    emit $ LLVM.Cbr (LLVM.Reg r1) (LLVM.Reg l3) (LLVM.Reg l2)
    setLabel l2
    x2 <- genExpr expr2
    l2' <- currentLabel
    emit $ LLVM.Br (LLVM.Reg l3)
    setLabel l3
    r3 <- newReg
    emit $ LLVM.Phi r3 LLVM.I1 (LLVM.LitInt 0) l1 x2 l2'
    return (LLVM.Reg r3)

genExpr (EOr _ expr1 expr2) = do
    x1 <- genExpr expr1
    l1 <- currentLabel
    r1 <- newReg
    l2 <- newLabel
    l3 <- newLabel
    emit $ LLVM.Cmp r1 LLVM.Eq LLVM.I1 x1 (LLVM.LitInt 1)
    emit $ LLVM.Cbr (LLVM.Reg r1) (LLVM.Reg l3) (LLVM.Reg l2)
    setLabel l2
    x2 <- genExpr expr2
    l2' <- currentLabel
    emit $ LLVM.Br (LLVM.Reg l3)
    setLabel l3
    r3 <- newReg
    emit $ LLVM.Phi r3 LLVM.I1 (LLVM.LitInt 1) l1 x2 l2'
    return (LLVM.Reg r3)

genExpr (EApp (_, Just typ) (Ident fname) args) = do
    argValues <- mapM genExpr args
    let argTypes = map (typeToLLVM . typeOf) args
    let cargs = map (uncurry LLVM.Carg) $ zip argTypes argValues
    let lltype = typeToLLVM typ
    let llid = LLVM.Ident ('@':fname)
    case typ of
        Void _ -> do
            emit $ LLVM.VCall lltype llid cargs
            return (LLVM.LitInt 0)  -- XXX ugly, but will be ignored
        _ -> do
            r <- newReg
            emit $ LLVM.Call r lltype llid cargs
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
mulOp2LLVM (Mod _) = LLVM.Rem
