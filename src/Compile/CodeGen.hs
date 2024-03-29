{-# LANGUAGE LambdaCase #-}

module Compile.CodeGen where
import qualified LLVM
import qualified Data.Map as Map
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Control.Monad (void, forM_, when, unless)
import Control.Monad.State
import Parsing.AbsLatte
import Errors.LatteError
import Semantics.TypeInfo
import Semantics.TypeCheck (returns) -- TODO move to different module and maybe rename
import Compile.Monad


compile :: (Program (PosInfo, TypeInfo), TypeEnv) -> Either (LatteError PType) LLVM.Module
compile (prog, te) = runGen te $ genProg prog


globDecls :: [LLVM.TopDef]
globDecls = [
    LLVM.FunDec LLVM.Void
                (LLVM.Ident "@printInt")
                [LLVM.i64],
    LLVM.FunDec LLVM.Void
                (LLVM.Ident "@printString")
                [LLVM.Ptr LLVM.i8],
    LLVM.FunDec LLVM.i64
                (LLVM.Ident "@readInt")
                [],
    LLVM.FunDec (LLVM.Ptr LLVM.i8)
                (LLVM.Ident "@readString")
                [],
    LLVM.FunDec (LLVM.Void)
                (LLVM.Ident "@error")
                [],
    LLVM.FunDec LLVM.Void
                (LLVM.Ident "@llvm.memcpy.p0i8.p0i8.i64")
                [LLVM.Ptr LLVM.i8, LLVM.Ptr LLVM.i8, LLVM.i64, LLVM.i32, LLVM.i1],
    LLVM.FunDec LLVM.i64
                (LLVM.Ident "@strlen")
                [LLVM.Ptr LLVM.i8],
    LLVM.FunDec (LLVM.Ptr LLVM.i8)
                (LLVM.Ident "@malloc")
                [LLVM.i64]
    ]
    
genProg :: Program (PosInfo, TypeInfo) -> LLGen ()
genProg (Program _ defs) = do
    tenv <- gets typeEnv
    mapM_ addClass $ Map.toList tenv
    mapM_ addDecl globDecls
    mapM_ genTopDef defs
  where
    addClass ((Ident name), (_, members)) = addDecl $
        LLVM.TypeDef (LLVM.Ident $ "%struct." ++ name)
                     (LLVM.Struct $ map (typeToLLVM . snd) members)


genTopDef :: TopDef (PosInfo, TypeInfo) -> LLGen ()
genTopDef (FnDef _ (FunDef _ retType fname args body@(Block _ stmts))) = do
    beginFunction retType fname args
    mapM_ genStmt stmts
    endFunction

genTopDef (ClsDef _ _ _ _) = return ()


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
            addVar typ ident
            case typ of
                Str _ -> genStmt $ Ass info (LVar info ident) (EString (nopos, Nothing) "")
                Int _ -> genStmt $ Ass info (LVar info ident) (ELitInt (nopos, Nothing) 0)
                Bool _ -> genStmt $ Ass info (LVar info ident) (ELitFalse (nopos, Nothing))
                -- TODO default initialization for other types?
                _ -> return ()
        Init info ident expr -> do
            x <- genExpr expr
            addVar typ ident
            setVar ident x )

genStmt (Ass _ (LVar _ ident) expr) = do
    x <- genExpr expr
    setVar ident x

genStmt (Ass _ (LInd (_, Just typ) arrExpr indExpr) expr) = do
    ind <- genExpr indExpr
    struct <- genExpr arrExpr
    checkBounds struct ind
    arr <- extractvalue 0 struct
    ptr <- getelementptr arr ind
    val <- genExpr expr
    store val ptr

genStmt (Ass _ (LMem (_, Just typ) objExpr mem) expr) = do
    x <- genExpr expr
    ptr <- genExpr objExpr
    let TCls _ cls = typeOf objExpr
    struct <- load ptr
    tenv <- gets typeEnv
    let Just (_, members) = Map.lookup cls tenv
    struct <- insertvalue struct x $ LLVM.litI64 $ toInteger $ findIndex members
    store struct ptr
  where
    findIndex l = fromJust $ elemIndex mem $ map fst l

genStmt (Incr _ (LVar _ ident)) = do
    val <- getVar ident
    r <- add val $ LLVM.litI64 1
    setVar ident r

genStmt (Decr _ (LVar _ ident)) = do
    val <- getVar ident
    r <- sub val $ LLVM.litI64 1
    setVar ident r

genStmt (Ret _ expr) = do
    x <- genExpr expr
    let t = typeToLLVM $ typeOf expr
    emit $ LLVM.Ret t x

genStmt (VRet _) = do
    emit $ LLVM.VRet

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

genStmt (For (pos, _) typ ident expr stmt) = do
    struct <- genExpr expr
    len <- extractvalue 1 struct
    arr <- extractvalue 0 struct
    lcurrent <- currentLabel
    lcond <- newLabel
    lloop <- newLabel
    lend <- newLabel
    br lcond
    setLabel lloop
    r <- newReg
    let i = LLVM.Reg LLVM.i64 r
    elem <- getelementptr arr i >>= load
    iinc <- add i (LLVM.litI64 1)
    newScope
    addVar typ ident
    setVar ident elem
    genStmt stmt
    endScope
    lafter <- currentLabel
    br lcond
    setLabel lcond
    emit $ LLVM.Phi r LLVM.i64 (LLVM.litI64 0) lcurrent iinc lafter
    cond <- cmp LLVM.Ge i len
    cbr cond lend lloop
    setLabel lend

genStmt (SExp _ expr) = void $ genExpr expr


genExpr :: Expr (PosInfo, TypeInfo) -> LLGen LLVM.Operand
genExpr (EVar _ ident) = do
    getVar ident

genExpr (EMem _ objExpr mem) = do
    ptr <- genExpr objExpr
    case typeOf objExpr of
        Arr _ _ -> extractvalue 1 ptr
        TCls _ cls -> do
            tenv <- gets typeEnv
            let Just (_, members) = Map.lookup cls tenv
            struct <- load ptr
            extractvalue (findIndex members) struct
  where
    findIndex l = fromJust $ elemIndex mem $ map fst l

genExpr (EInd _ arrExpr indExpr) = do
    ind <- genExpr indExpr
    struct <- genExpr arrExpr
    checkBounds struct ind
    arr <- extractvalue 0 struct
    getelementptr arr ind >>= load

genExpr (ENew _ clstyp) = do
    let LLVM.Ptr typ@(LLVM.NamedType name) = typeToLLVM clstyp
    structType <- getNamedType name
    Just ptrI8 <- call (LLVM.Ptr LLVM.i8) (LLVM.Ident "@malloc") [LLVM.litI64 $ sizeof structType]
    ptr <- bitcast ptrI8 $ LLVM.Ptr typ
    newstruct <- store (LLVM.ConstOperand $ LLVM.Undef typ) ptr
    return ptr

genExpr (ENull _ ident) = do
    let typ = typeToLLVM $ TCls nopos ident
    return $ LLVM.ConstOperand $ LLVM.Null $ LLVM.Ptr typ

genExpr (ELitTrue _) = return $ LLVM.litI1 1

genExpr (ELitFalse _) = return $ LLVM.litI1 0

genExpr (ELitInt _ n) = return $ LLVM.litI64 n

genExpr (EString _ s) = do
    r <- newReg
    glob <- addStr s r
    emit $ LLVM.Bitcast r glob (LLVM.Ptr LLVM.i8)
    return $ LLVM.Reg (LLVM.Ptr LLVM.i8) r

genExpr (EAdd (_, Just typ) expr1 op expr2) = case typ of
    Int _ -> do
        x1 <- genExpr expr1
        x2 <- genExpr expr2
        genBinop (addOp2LLVM op) x1 x2
    Str _ -> do
        x1 <- genExpr expr1
        x2 <- genExpr expr2
        l1 <- callStrlen x1
        l2 <- callStrlen x2
        sum <- add l1 l2
        len <- add sum $ LLVM.litI64 1
        Just res <- call (LLVM.Ptr LLVM.i8) (LLVM.Ident "@malloc") [len]
        _ <- call LLVM.Void (LLVM.Ident "@llvm.memcpy.p0i8.p0i8.i64")
                            [res, x1, l1, LLVM.litI32 0, LLVM.litI1 1]
        ptr <- getelementptr res l1
        l2plus1 <- add l2 $ LLVM.litI64 1
        _ <- call LLVM.Void (LLVM.Ident "@llvm.memcpy.p0i8.p0i8.i64")
                            [ptr, x2, l2plus1, LLVM.litI32 0, LLVM.litI1 1]
        return res

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
    sub (LLVM.litI1 1) x

genExpr (Neg _ expr) = do
    x <- genExpr expr
    r <- newReg
    mul x $ LLVM.litI64 (-1)

genExpr (EAnd _ expr1 expr2) = do
    x1 <- genExpr expr1
    l1 <- currentLabel
    l2 <- newLabel
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

genExpr (EArr _ t expr) = do
    len <- genExpr expr
    let elemType = typeToLLVM t
    size <- mul len $ LLVM.litI64 $ sizeof elemType
    Just arr <- call (LLVM.Ptr LLVM.i8) (LLVM.Ident "@malloc") [size]
    ptr <- bitcast arr $ LLVM.Ptr elemType
    tmp <- insertvalue (LLVM.ConstOperand (LLVM.Undef $ LLVM.Struct [LLVM.Ptr elemType, LLVM.i64])) ptr $ LLVM.litI64 0
    insertvalue tmp len $ LLVM.litI64 1

genExpr (EApp (_, Just typ) (Ident fname) args) = do
    argValues <- mapM genExpr args
    let lltype = typeToLLVM typ
    let llid = LLVM.Ident ('@':fname)
    maybeRet <- call lltype llid argValues
    case maybeRet of
        Nothing -> return $ LLVM.ConstOperand $ LLVM.Undef LLVM.Void
        Just ret -> return ret


checkBounds :: LLVM.Operand -> LLVM.Operand -> LLGen ()
checkBounds arrayStruct index = do
    lcurrent <- currentLabel
    lelse <- newLabel
    lerror <- newLabel
    lok <- newLabel
    c1 <- cmp LLVM.Lt index $ LLVM.litI64 0
    cbr c1 lerror lelse
    setLabel lelse
    len <- extractvalue 1 arrayStruct
    c2 <- cmp LLVM.Ge index len
    cbr c2 lerror lok
    setLabel lerror
    _ <- call LLVM.Void (LLVM.Ident "@error") []
    br lok -- TODO unreachable
    setLabel lok


genBinop :: LLVM.Binop -> LLVM.Operand -> LLVM.Operand -> LLGen LLVM.Operand
genBinop op x y = do
    r <- newReg
    let typ = LLVM.operandType x
    emit $ LLVM.Bin r op typ x y
    return $ LLVM.Reg typ r

add :: LLVM.Operand -> LLVM.Operand -> LLGen LLVM.Operand
add = genBinop LLVM.Add

sub :: LLVM.Operand -> LLVM.Operand -> LLGen LLVM.Operand
sub = genBinop LLVM.Sub

mul :: LLVM.Operand -> LLVM.Operand -> LLGen LLVM.Operand
mul = genBinop LLVM.Mul


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


call :: LLVM.Type -> LLVM.Ident -> [LLVM.Operand] -> LLGen (Maybe LLVM.Operand)
call typ fname args =
    let cargs = map (\arg -> LLVM.Carg (LLVM.operandType arg) arg) args in
    if typ == LLVM.Void then do
        emit $ LLVM.VCall typ fname cargs
        return Nothing
    else do
        r <- newReg
        emit $ LLVM.Call r typ fname cargs
        return $ Just $ LLVM.Reg typ r


phi :: (LLVM.Operand, LLVM.Ident) -> (LLVM.Operand, LLVM.Ident) -> LLGen LLVM.Operand
phi (v1, l1) (v2, l2) = do
    r <- newReg
    let typ = LLVM.operandType v1
    emit $ LLVM.Phi r typ v1 l1 v2 l2
    return $ LLVM.Reg typ r


getelementptr :: LLVM.Operand -> LLVM.Operand -> LLGen LLVM.Operand
getelementptr ptr idx = do
    res <- newReg
    let LLVM.Ptr typ = LLVM.operandType ptr
    emit $ LLVM.GEP res typ ptr idx 
    return $ LLVM.Reg (LLVM.Ptr typ) res


load :: LLVM.Operand -> LLGen LLVM.Operand
load ptr = do
    res <- newReg
    let LLVM.Ptr typ = LLVM.operandType ptr
    emit $ LLVM.Load res typ ptr
    return $ LLVM.Reg typ res

store :: LLVM.Operand -> LLVM.Operand -> LLGen ()
store val ptr = emit $ LLVM.Store (LLVM.operandType val) val ptr

alloca :: LLVM.Type -> LLGen LLVM.Operand
alloca typ = do
    ptr <- newReg
    emit $ LLVM.Alloc ptr typ
    return $ LLVM.Reg (LLVM.Ptr typ) ptr


insertvalue :: LLVM.Operand -> LLVM.Operand -> LLVM.Operand -> LLGen LLVM.Operand
insertvalue struct val idx = do
    res <- newReg
    emit $ LLVM.Insertval res struct val idx
    return $ LLVM.Reg (LLVM.operandType struct) res


extractvalue :: Int -> LLVM.Operand -> LLGen LLVM.Operand
extractvalue idx struct = do
    res <- newReg
    emit $ LLVM.Extractval res struct $ LLVM.litI64 $ toInteger idx
    case LLVM.operandType struct of
        LLVM.Struct types -> return $ LLVM.Reg (types !! idx) res
        LLVM.NamedType name -> do
            LLVM.Struct types <- getNamedType name
            return $ LLVM.Reg (types !! idx) res


bitcast :: LLVM.Operand -> LLVM.Type -> LLGen LLVM.Operand
bitcast val typ = do
    r <- newReg
    emit $ LLVM.Bitcast r val typ
    return $ LLVM.Reg typ r


sizeof :: LLVM.Type -> Integer
sizeof (LLVM.I 1) = 1
sizeof (LLVM.I n) = toInteger (n `div` 8)
sizeof (LLVM.Ptr _) = 8
sizeof (LLVM.Struct types) = foldl (+) 0 $ map sizeof types

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
