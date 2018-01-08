module LLVM.Printer where
import LLVM.AST


printTree = printModule

printModule :: Module -> String
printModule (Module defs) = unlines $ map printTopDef defs


printTopDef :: TopDef -> String
printTopDef (FunDef typ ident args body) = unlines [
    ("define" <+> (printType typ) <+> (printIdent ident) <+> (printArgs args) <+> "{"),
    unlines $ map printInstr body,
    "}"]

printTopDef (FunDec typ ident args) = "declare" <+> (printType typ)
                                                <+> (printIdent ident)
                                                <+> (printArgs args)

printTopDef (ConstDef name typ init) = (printIdent name) <=> "constant" <+> (printType typ)
                                                                        <+> (printOperand init)


printType :: Type -> String
printType I32 = "i32"
printType I8 = "i8"
printType I1 = "i1"
printType Void = "void"
printType (Array size typ) = "[" ++ (show size) <+> "x" <+> printType typ ++ "]"
printType (Ptr typ) = (printType typ) ++ "*"


printIdent :: Ident -> String
printIdent (Ident str) = str


printArgs :: [Arg] -> String
printArgs args = '(':(printArgs' args) ++ ")"
printArgs' [] = ""
printArgs' [Arg typ ident] = (printType typ) <+> (printIdent ident)
printArgs' (a:as) = (printArgs' [a]) ++ ", " ++ (printArgs' as)


printInstr :: Instr -> String
printInstr (Ret typ op) = "ret" <+> (printType typ) <+> (printOperand op)

printInstr (VRet) = "ret void"

printInstr (Br op) = "br label" <+> (printOperand op)

printInstr (Cbr cond op1 op2) = "br i1" <+> (printOperand cond) ++ ", label" <+> (printOperand op1)
                                                                ++ ", label" <+> (printOperand op2)

printInstr (Bin res binop typ arg1 arg2) = (printIdent res) <=> (printBinop binop) <+> (printType typ)
                                                            <+> (printOperand arg1) ++ ", " ++ (printOperand arg2)

printInstr (Phi res typ op1 l1 op2 l2) = (printIdent res) <=> "phi" <+> (printType typ) <+> "["
                                                          ++ (printOperand op1) ++ ", " ++ (printIdent l1) ++ "], ["
                                                          ++ (printOperand op2) ++ ", " ++ (printIdent l2) ++ "]"

printInstr (Cmp res cmpop typ arg1 arg2) = (printIdent res) <=> (printCmpop cmpop) <+> (printType typ)
                                                            <+> (printOperand arg1) ++ ", " ++ (printOperand arg2)

printInstr (Call res typ fname args) = (printIdent res) <=> "call" <+> (printType typ)
                                                        <+> (printIdent fname) <+> (printCargs args)

printInstr (VCall typ fname args) = "call" <+> (printType typ)
                                           <+> (printIdent fname)
                                           <+> (printCargs args)

printInstr (Label (Ident (_:t))) = (printIdent $ Ident t) ++ ":"

printInstr (Bitcast res typ1 const typ2) = (printIdent res) <=> "bitcast" <+> (printType typ1)
                                                                          <+> (printIdent const)
                                                                          <+> "to" <+> (printType typ2)


printCargs :: [Carg] -> String
printCargs args = '(':(printCargs' args) ++ ")"
printCargs' [] = ""
printCargs' [Carg typ arg] = (printType typ) <+> (printOperand arg)
printCargs' (a:as) = (printCargs' [a]) ++ ", " ++ (printCargs' as)


printCmpop :: Cmpop -> String
printCmpop Eq = "eq"
printCmpop Ne = "ne"
printCmpop Gt = "gt"
printCmpop Ge = "ge"
printCmpop Lt = "lt"
printCmpop Le = "le"


printBinop :: Binop -> String
printBinop Add = "add"
printBinop Sub = "sub"
printBinop Mul = "mul"
printBinop Div = "idiv"


printOperand :: Operand -> String
printOperand (Reg ident) = printIdent ident
printOperand (LitInt n) = show n
printOperand (LitStr s) = 'c':((init s) ++ "\\00\"")

(<+>) :: String -> String -> String
a <+> b = a ++ " " ++ b

(<=>) :: String -> String -> String
a <=> b = a ++ " = " ++ b
