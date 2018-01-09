{-# LANGUAGE LambdaCase #-}

module Compile.CodeGen where
import qualified LLVM
import qualified Data.Map as Map
import Control.Monad (void, forM_, forM, liftM)
import Data.Maybe (fromJust)
import Parsing.AbsLatte
import Errors.LatteError
import Semantics.TypeCheck (TypeInfo (..))
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
                []
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
    let t = typeToLLVM $ typeOfExpr expr
    emit $ LLVM.Ret t x

genStmt (VRet _) = do
    emit $ LLVM.VRet

genStmt (Cond _ expr stmt) = do
    x <- genExpr expr
    lold <- currentLabel
    ltrue <- newLabel
    lfalse <- newLabel
    emit $ LLVM.Cbr x (LLVM.Reg ltrue) (LLVM.Reg lfalse)
    beginBasicBlock ltrue
    genStmt stmt
    (phis, lnew) <- endBasicBlock
    emit $ LLVM.Br (LLVM.Reg lfalse)
    setLabel lfalse
    forM_ (Map.assocs phis) (\(ident, (old, new)) -> do
        r <- newReg
        -- TODO type
        emit $ LLVM.Phi r LLVM.I32 old lold new lnew
        setVar ident (LLVM.Reg r) )

genStmt (CondElse _ expr trueStmt falseStmt) = do
    x <- genExpr expr
    lold <- currentLabel
    ltrue <- newLabel
    lfalse <- newLabel
    lafter <- newLabel
    emit $ LLVM.Cbr x (LLVM.Reg ltrue) (LLVM.Reg lfalse)
    beginBasicBlock ltrue
    genStmt trueStmt
    (phisTrue, endLabelTrue) <- endBasicBlock
    emit $ LLVM.Br (LLVM.Reg lafter)
    beginBasicBlock lfalse
    genStmt falseStmt
    (phisFalse, endLabelFalse) <- endBasicBlock
    emit $ LLVM.Br (LLVM.Reg lafter)
    setLabel lafter
    -- TODO XXX boilerplate
    let phis = Map.unionWith (\t f -> (snd t, snd f)) phisTrue phisFalse
    forM_ (Map.assocs phis) (\(ident, (true, false)) -> do
        r <- newReg
        -- TODO type
        emit $ LLVM.Phi r LLVM.I32 true endLabelTrue false endLabelFalse
        setVar ident (LLVM.Reg r) )

genStmt (While _ expr stmt) = do
    lold <- currentLabel
    lcond <- newLabel
    lloop <- newLabel
    lafter <- newLabel
    emit $ LLVM.Br (LLVM.Reg lcond)
    let usd = usedVars stmt
    mapping <- (liftM Map.fromList) $ forM usd (\ident -> do
        r <- newReg
        prev <- getVar ident
        setVar ident (LLVM.Reg r)
        return (ident, prev) )
    beginBasicBlock lloop
    genStmt stmt
    (phis, lnew) <- endBasicBlock
    emit $ LLVM.Br (LLVM.Reg lcond)
    setLabel lcond
    forM_ (Map.assocs phis) (\(ident, ((LLVM.Reg r), new)) -> do
        emit $ LLVM.Phi r LLVM.I32 (mapping Map.! ident) lold new lnew
        setVar ident (LLVM.Reg r) )
    x <- genExpr expr
    emit $ LLVM.Cbr x (LLVM.Reg lloop) (LLVM.Reg lafter)
    setLabel lafter

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
    -- XXX
    emit $ LLVM.Cmp r (cmpOp2LLVM op) LLVM.I32 x1 x2
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
    setLabel l2
    x2 <- genExpr expr2
    emit $ LLVM.Br (LLVM.Reg l3)
    setLabel l3
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
    setLabel l2
    x2 <- genExpr expr2
    emit $ LLVM.Br (LLVM.Reg l3)
    setLabel l3
    r3 <- newReg
    emit $ LLVM.Phi r3 LLVM.I1 (LLVM.LitInt 1) l1 x2 l2
    return (LLVM.Reg r3)

genExpr (EApp (_, (Just typ, _)) (Ident fname) args) = do
    argValues <- mapM genExpr args
    let argTypes = map (typeToLLVM . typeOfExpr) args
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

-- rename?
usedVars :: Stmt a -> [Ident]
usedVars (BStmt _ (Block _ stmts)) = concatMap usedVars stmts
usedVars (Ass _ (LhsVar _ ident) _) = [ident]
usedVars (Incr _ (LhsVar _ ident)) = [ident]
usedVars (Decr _ (LhsVar _ ident)) = [ident]
usedVars (Cond _ _ stmt) = usedVars stmt
usedVars (CondElse _ _ s1 s2) = (usedVars s1) ++ (usedVars s2)
usedVars (While _ _ stmt) = usedVars stmt
usedVars _ = []
