module Semantics.TypeError (
    typeMismatch,
    undeclaredVariable,
    multipleDeclarations,
    noReturn,
    otherError
)
where
import Data.List (intersperse)
import Errors.LatteError
import Parsing.AbsLatte


(<+>) :: String -> String -> String
x <+> "" = x
x <+> y = x ++ " " ++ y


pType :: Type a -> String
pType (Int _) = "int"
pType (Str _) = "str"
pType (Bool _) = "boolean"
pType (Void _) = "void"
pType (Fun _ r a) = (pType r) ++ "(" ++ (concat (intersperse "," (map pType a))) ++ ")"
pType (Arr _ t) = (pType t) ++ "[]"
pType (TCls _ (Ident name)) = name


pPos :: PosInfo -> String
pPos (Just (lno, cno)) = "around line:" <+> (show lno) ++ ", col:" <+> (show cno)
pPos Nothing = ""


typeMismatch :: PType -> PType -> PosInfo -> LatteError PType
typeMismatch t1 t2 pos = TErr $ TypeMismatch
    ("TypeError: type mismatch, got" <+> (pType t1) ++ ", expected" <+> (pType t2) <+> pPos pos)
    t1 t2


undeclaredVariable :: Ident -> PosInfo -> LatteError PType
undeclaredVariable (Ident name) pos = TErr $ UndeclaredVariable $
    "TypeError: undeclared variable" <+> name <+> pPos pos


multipleDeclarations :: Ident -> PosInfo -> LatteError PType
multipleDeclarations (Ident name) pos = TErr $ MultipleDeclarations $
    "TypeError: multiple declarations of variable" <+> name <+> pPos pos


noReturn :: Ident -> PosInfo -> LatteError PType
noReturn (Ident name) pos = TErr $ NoReturn $
    "TypeError: control may reach end of non-void function" <+> name <+> "declared at" <+> pPos pos


otherError :: Maybe String -> PosInfo -> LatteError PType
otherError mMsg pos = TErr $ OtherError $ case mMsg of
    Nothing -> "TypeError occured" <+> pPos pos
    Just msg -> "TypeError:" <+> msg <+> pPos pos

