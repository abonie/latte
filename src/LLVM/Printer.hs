module LLVM.Printer where
import LLVM.AST
import Data.List (intercalate)


printTree = printModule

printModule :: Module -> String
printModule (Module defs) = unlines $ map printTopDef defs


printTopDef :: TopDef -> String
printTopDef (FunDef typ ident args body) = unlines [
    ("define" <+> (printType typ) <+> (printIdent ident) <+> (pParens printArg args) <+> "{"),
    unlines $ map printInstr body,
    "}"]

printTopDef (FunDec typ ident args) = "declare" <+> (printType typ)
                                                <+> (printIdent ident)
                                                <+> (pParens printType args)

printTopDef (ConstDef name typ init) = '@':(printIdent name) <=> "constant" <+> (printType typ)
                                                                        <+> (printConst init)


printType :: Type -> String
printType (I n) = 'i':(show n)
printType Void = "void"
printType (Array size typ) = "[" ++ (show size) <+> "x" <+> printType typ ++ "]"
printType (Ptr typ) = (printType typ) ++ "*"
printType TLabel = "label"
printType (Struct types) = '{':(intercalate ", " $ map printType types) ++ "}"


printIdent :: Ident -> String
printIdent (Ident str) = str


printArg :: Arg -> String
printArg (Arg typ ident) = (printType typ) <+> (printIdent ident)


printInstr :: Instr -> String
printInstr (Ret typ op) = "ret" <+> (printType typ) <+> (printOperand op)

printInstr (VRet) = "ret void"

printInstr (Alloc res typ) = (printIdent res) <=> "alloca" <+> (printType typ)

printInstr (Load res typ ptr) = (printIdent res) <=> "load" <+> (printType typ) ++ ", "
                                                            ++ (printType $ Ptr typ) <+> (printOperand ptr)

printInstr (Store typ val ptr) = "store" <+> (printType typ) <+> (printOperand val) ++ ", "
                                     ++ (printType $ Ptr typ) <+> (printOperand ptr)

printInstr (Br op) = "br label" <+> (printOperand op)

printInstr (Cbr cond op1 op2) = "br i1" <+> (printOperand cond) ++ ", label" <+> (printOperand op1)
                                                                ++ ", label" <+> (printOperand op2)

printInstr (Bin res binop typ arg1 arg2) = (printIdent res) <=> (printBinop binop) <+> (printType typ)
                                                            <+> (printOperand arg1) ++ ", " ++ (printOperand arg2)

printInstr (Phi res typ op1 l1 op2 l2) = (printIdent res) <=> "phi" <+> (printType typ) <+> "["
                                                          ++ (printOperand op1) ++ ", " ++ (printIdent l1) ++ "], ["
                                                          ++ (printOperand op2) ++ ", " ++ (printIdent l2) ++ "]"

printInstr (Cmp res cmpop typ arg1 arg2) = (printIdent res) <=> "icmp" <+> (printCmpop cmpop) <+> (printType typ)
                                                            <+> (printOperand arg1) ++ ", " ++ (printOperand arg2)

printInstr (Call res typ fname args) = (printIdent res) <=> "call" <+> (printType typ)
                                                        <+> (printIdent fname) <+> (printCargs args)

printInstr (VCall typ fname args) = "call" <+> (printType typ)
                                           <+> (printIdent fname)
                                           <+> (printCargs args)

printInstr (Label (Ident (_:t))) = (printIdent $ Ident t) ++ ":"

printInstr (Bitcast res op typ) = (printIdent res) <=> "bitcast" <+> (printType $ operandType op)
                                                                          <+> (printOperand op)
                                                                          <+> "to" <+> (printType typ)

printInstr (GEP res typ ptr idx) = (printIdent res) <=> "getelementptr" <+> (printType typ) ++ ", "
                                                                        ++ (printType $ Ptr typ)
                                                                        <+> (printOperand ptr) ++ ", "
                                                                        ++ (printType i64)
                                                                        <+> (printOperand idx)

printInstr (Insertval res to val idx) = let (t1, t2) = (operandType to, operandType val) in
    (printIdent res) <=> "insertvalue" <+> (printType t1) <+> (printOperand to) ++ ", "
                                        ++ (printType t2) <+> (printOperand val) ++ ", "
                                                           ++ (printOperand idx)

printInstr (Extractval res struct idx) = let typ = operandType struct in
    (printIdent res) <=> "extractvalue" <+> (printType typ) <+> (printOperand struct) ++ ", " ++ (printOperand idx)

printCargs :: [Carg] -> String
printCargs args = '(':(printCargs' args) ++ ")"
printCargs' [] = ""
printCargs' [Carg typ arg] = (printType typ) <+> (printOperand arg)
printCargs' (a:as) = (printCargs' [a]) ++ ", " ++ (printCargs' as)


printCmpop :: Cmpop -> String
printCmpop Eq = "eq"
printCmpop Ne = "ne"
printCmpop Gt = "sgt"
printCmpop Ge = "sge"
printCmpop Lt = "slt"
printCmpop Le = "sle"


printBinop :: Binop -> String
printBinop Add = "add"
printBinop Sub = "sub"
printBinop Mul = "mul"
printBinop Div = "sdiv"
printBinop Rem = "srem"


printOperand :: Operand -> String
printOperand (Reg _ ident) = printIdent ident
printOperand (ConstOperand c) = printConst c

printConst :: Constant -> String
printConst (Int _ n) = show n
printConst (Str "") = "c\"\\00\""
printConst (Str s) = 'c':((init s) ++ "\\00\"")
printConst (Undef _) = "undef"
printConst (Global _ ident) = '@':(printIdent ident)

(<+>) :: String -> String -> String
a <+> b = a ++ " " ++ b

(<=>) :: String -> String -> String
a <=> b = a ++ " = " ++ b

pParens :: (a -> String) -> [a] -> String
pParens p l = '(':(intercalate ", " $ map p l) ++ ")"

