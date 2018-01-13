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
                [LLVM.Arg LLVM.i32 (LLVM.Ident "%x")],
    LLVM.FunDec LLVM.Void
                (LLVM.Ident "@printString")
                [LLVM.Arg (LLVM.Ptr LLVM.i8) (LLVM.Ident "%s")],
    LLVM.FunDec LLVM.i32
                (LLVM.Ident "@readInt")
                [],
    LLVM.FunDec (LLVM.Ptr LLVM.i8)
                (LLVM.Ident "@readString")
                [],
    LLVM.FunDec LLVM.Void
                (LLVM.Ident "@llvm.memcpy.p0i8.p0i8.i64")
                [LLVM.Arg (LLVM.Ptr LLVM.i8) (LLVM.Ident "%dst"),
                 LLVM.Arg (LLVM.Ptr LLVM.i8) (LLVM.Ident "%src"),
                 LLVM.Arg LLVM.i64 (LLVM.Ident "%len"),
                 LLVM.Arg LLVM.i32 (LLVM.Ident "%algn"),
                 LLVM.Arg LLVM.i1 (LLVM.Ident "%volatile")],
    LLVM.FunDec LLVM.i64
                (LLVM.Ident "@strlen")
                [LLVM.Arg (LLVM.Ptr LLVM.i8) (LLVM.Ident "%str")],
    LLVM.FunDec (LLVM.Ptr LLVM.i8)
                (LLVM.Ident "@malloc")
                [LLVM.Arg LLVM.i64 (LLVM.Ident "%n")]
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
    val <- getVar ident
    r <- add val $ LLVM.litI32 1
    setVar ident r

genStmt (Decr _ (LhsVar _ ident)) = do
    val <- getVar ident
    r <- sub val $ LLVM.litI32 1
    setVar ident r

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
    _ <- cbr x ltrue lfalse
    setLabel ltrue
    genStmt stmt
    unless (returns stmt) (br lfalse)
    setLabel lfalse

genStmt (CondElse _ expr tStmt fStmt) = do
    x <- genExpr expr
    ltrue <- newLabel
    lfalse <- newLabel
    lafter <- newLabel
    cbr x ltrue lfalse
    setLabel ltrue
    genStmt tStmt
    unless (returns tStmt) (br lafter)
    setLabel lfalse
    genStmt fStmt
    unless (returns fStmt) (br lafter)
    setLabel lafter

genStmt (While _ expr stmt) = do
    lcond <- newLabel
    lloop <- newLabel
    lafter <- newLabel
    br lcond
    setLabel lcond
    x <- genExpr expr
    cbr x lloop lafter
    setLabel lloop
    genStmt stmt
    br lcond
    setLabel lafter

genStmt (SExp _ expr) = void $ genExpr expr


genExpr :: Expr (PosInfo, TypeInfo) -> LLGen LLVM.Operand
genExpr (EVar _ ident) = do
    getVar ident

genExpr (ELitTrue _) = return $ LLVM.litI1 1

genExpr (ELitFalse _) = return $ LLVM.litI1 0

genExpr (ELitInt _ n) = return $ LLVM.litI32 n

genExpr (EString _ s) = do
    r <- newReg
    glob <- addStr s r
    let len = max 1 $ (length s) - 1
    emit $ LLVM.Bitcast r (LLVM.Ptr (LLVM.Array len LLVM.i8)) glob (LLVM.Ptr LLVM.i8)
    return $ LLVM.Reg (LLVM.Ptr LLVM.i8) r

genExpr (EAdd (_, Just typ) expr1 op expr2) = case typ of
    Int _ -> do
        x1 <- genExpr expr1
        x2 <- genExpr expr2
        genBinop (addOp2LLVM op) x1 x2
    Str _ -> do
        -- TODO check if operator is Add ? type checking ensures that
        x1 <- genExpr expr1
        x2 <- genExpr expr2
        l1Reg <- callStrlen x1
        l2Reg <- callStrlen x2
        sum <- add l1Reg l2Reg
        len <- add sum $ LLVM.litI64 1
        res <- newReg
        emit $ LLVM.Call res (LLVM.Ptr LLVM.i8) (LLVM.Ident "@malloc") [LLVM.Carg LLVM.i64 len]
        let args1 = [
                LLVM.Carg (LLVM.Ptr LLVM.i8) (LLVM.Reg (LLVM.Ptr LLVM.i8) res),
                LLVM.Carg (LLVM.Ptr LLVM.i8) x1,
                LLVM.Carg LLVM.i64 l1Reg,
                LLVM.Carg LLVM.i32 (LLVM.litI32 0),
                LLVM.Carg LLVM.i1 (LLVM.litI1 1)
                ]
        emit $ LLVM.VCall LLVM.Void (LLVM.Ident "@llvm.memcpy.p0i8.p0i8.i64") args1
        ptr <- newReg
        emit $ LLVM.GEP ptr LLVM.i8 (LLVM.Reg (LLVM.Ptr LLVM.i8) res) l1Reg
        l2plus1 <- add l2Reg $ LLVM.litI64 1
        let args2 = [
                LLVM.Carg (LLVM.Ptr LLVM.i8) (LLVM.Reg (LLVM.Ptr LLVM.i8) ptr),
                LLVM.Carg (LLVM.Ptr LLVM.i8) x2,
                LLVM.Carg LLVM.i64 l2plus1,
                LLVM.Carg LLVM.i32 (LLVM.litI32 0),
                LLVM.Carg LLVM.i1 (LLVM.litI1 1)
                    ]
        emit $ LLVM.VCall LLVM.Void (LLVM.Ident "@llvm.memcpy.p0i8.p0i8.i64") args2
        return $ LLVM.Reg (LLVM.Ptr LLVM.i8) res

genExpr (EMul _ expr1 op expr2) = do
    x1 <- genExpr expr1
    x2 <- genExpr expr2
    genBinop (mulOp2LLVM op) x1 x2

genExpr (ERel _ expr1 op expr2) = do
    x1 <- genExpr expr1
    x2 <- genExpr expr2
    cmp (cmpOp2LLVM op) x1 x2

genExpr (Not _ expr) = do
    x <- genExpr expr
    r <- newReg
    emit $ LLVM.Bin r LLVM.Sub LLVM.i1 (LLVM.litI1 1) x
    return (LLVM.Reg LLVM.i1 r)

genExpr (Neg _ expr) = do
    x <- genExpr expr
    r <- newReg
    emit $ LLVM.Bin r LLVM.Mul LLVM.i32 x $ LLVM.litI32 (-1)
    return (LLVM.Reg LLVM.i32 r)

genExpr (EAnd _ expr1 expr2) = do
    x1 <- genExpr expr1
    l1 <- currentLabel
    l2 <- newLabel
    r1 <- newReg
    l3 <- newLabel
    r1 <- cmp LLVM.Eq x1 $ LLVM.litI1 0
    cbr r1 l3 l2
    setLabel l2
    x2 <- genExpr expr2
    l2' <- currentLabel
    br l3
    setLabel l3
    r3 <- newReg
    emit $ LLVM.Phi r3 LLVM.i1 (LLVM.litI1 0) l1 x2 l2'
    return (LLVM.Reg LLVM.i1 r3)

genExpr (EOr _ expr1 expr2) = do
    x1 <- genExpr expr1
    l1 <- currentLabel
    l2 <- newLabel
    l3 <- newLabel
    r1 <- cmp LLVM.Eq x1 $ LLVM.litI1 1
    cbr r1 l3 l2
    setLabel l2
    x2 <- genExpr expr2
    l2' <- currentLabel
    br l3
    setLabel l3
    r3 <- newReg
    emit $ LLVM.Phi r3 LLVM.i1 (LLVM.litI1 1) l1 x2 l2'
    return (LLVM.Reg LLVM.i1 r3)

genExpr (EApp (_, Just typ) (Ident fname) args) = do
    argValues <- mapM genExpr args
    let argTypes = map (typeToLLVM . typeOf) args
    let cargs = map (uncurry LLVM.Carg) $ zip argTypes argValues
    let lltype = typeToLLVM typ
    let llid = LLVM.Ident ('@':fname)
    case typ of
        Void _ -> do
            emit $ LLVM.VCall lltype llid cargs
            return (LLVM.ConstOperand $ LLVM.Undef LLVM.Void)
        _ -> do
            r <- newReg
            emit $ LLVM.Call r lltype llid cargs
            return (LLVM.Reg lltype r)


genBinop :: LLVM.Binop -> LLVM.Operand -> LLVM.Operand -> LLGen LLVM.Operand
genBinop op x y = do
    r <- newReg
    let typ = LLVM.operandType x
    emit $ LLVM.Bin r op typ x y
    return $ LLVM.Reg typ r

add :: LLVM.Operand -> LLVM.Operand -> LLGen LLVM.Operand
add x y = genBinop LLVM.Add x y

sub :: LLVM.Operand -> LLVM.Operand -> LLGen LLVM.Operand
sub x y = genBinop LLVM.Sub x y


cbr :: LLVM.Operand -> LLVM.Ident -> LLVM.Ident -> LLGen ()
cbr cond ltr lfs = emit $ LLVM.Cbr cond (LLVM.Reg LLVM.TLabel ltr) (LLVM.Reg LLVM.TLabel lfs)


br :: LLVM.Ident -> LLGen ()
br lab = emit $ LLVM.Br (LLVM.Reg LLVM.TLabel lab)


cmp :: LLVM.Cmpop -> LLVM.Operand -> LLVM.Operand -> LLGen LLVM.Operand
cmp op x y = do
    let t = LLVM.operandType x
    r <- newReg
    emit $ LLVM.Cmp r op t x y
    return $ LLVM.Reg LLVM.i1 r

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
