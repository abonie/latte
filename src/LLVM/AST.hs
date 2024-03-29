module LLVM.AST where


newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
data Module = Module [TopDef]
  deriving (Eq, Ord, Show, Read)

data TopDef
    = FunDef Type Ident [Arg] [Instr]
    | FunDec Type Ident [Type]
    | ConstDef Ident Type Constant
    | TypeDef Ident Type
  deriving (Eq, Ord, Show, Read)

data Arg = Arg Type Ident
  deriving (Eq, Ord, Show, Read)

data Instr
    = Ret Type Operand
    | VRet
    | Alloc Ident Type
    | Load Ident Type Operand
    | Store Type Operand Operand
    | Br Operand
    | Cbr Operand Operand Operand
    | Bin Ident Binop Type Operand Operand
    | Phi Ident Type Operand Ident Operand Ident
    | Call Ident Type Ident [Carg]
    | VCall Type Ident [Carg]
    | Cmp Ident Cmpop Type Operand Operand
    | Bitcast Ident Operand Type
    | GEP Ident Type Operand Operand
    | Insertval Ident Operand Operand Operand
    | Extractval Ident Operand Operand
    | Label Ident
  deriving (Eq, Ord, Show, Read)

data Carg = Carg Type Operand
  deriving (Eq, Ord, Show, Read)

data Binop = Add | Sub | Mul | Div | Rem
  deriving (Eq, Ord, Show, Read)

data Cmpop = Eq | Ne | Gt | Ge | Lt | Le
  deriving (Eq, Ord, Show, Read)

data Operand = Reg Type Ident | ConstOperand Constant
  deriving (Eq, Ord, Show, Read)

data Constant = Int Int Integer | Str String | Undef Type | Global Type Ident | Null Type
  deriving (Eq, Ord, Show, Read)

data Type
    = I Int
    | Void
    | Array Int Type
    | Ptr Type
    | TLabel
    | Struct [Type]
    | NamedType Ident
  deriving (Eq, Ord, Show, Read)

i1  = I 1
i8  = I 8
i32 = I 32
i64 = I 64

intLit width val = ConstOperand $ Int width val
litI64 = intLit 64
litI32 = intLit 32
litI8  = intLit 8
litI1  = intLit 1

operandType :: Operand -> Type
operandType (Reg t _) = t
operandType (ConstOperand c) = constType c

constType :: Constant -> Type
constType (Int i _) = I i
constType (Undef t) = t
constType (Str s) = Array (length s) i8
constType (Global t _) = t
