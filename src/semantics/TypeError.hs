{-# Language DeriveFunctor #-}

module TypeError where
import Data.List (intersperse)
import AbsLatte


data TypeError_ a = TypeMismatch a PType PType
               | UndeclaredVariable a
               | MultipleDeclarations a
               | NoReturn a
               | OtherError a
    deriving (Show, Eq, Functor)


type TypeError = TypeError_ String


(<+>) :: String -> String -> String
x <+> "" = x
x <+> y = x ++ " " ++ y


pPos :: PosInfo -> String
pPos (Just (lno, cno)) = "around line:" <+> (show lno) ++ ", col:" <+> (show cno)
pPos Nothing = ""


pType :: Type a -> String
pType (Int _) = "int"
pType (Str _) = "str"
pType (Bool _) = "bool"
pType (Void _) = "void"
pType (Fun _ r a) = (pType r) ++ "(" ++ (concat (intersperse "," (map pType a))) ++ ")"


typeMismatch :: PType -> PType -> PosInfo -> TypeError
typeMismatch t1 t2 pos = TypeMismatch
    ("TypeError: type mismatch, got" <+> (pType t1) ++ ", expected" <+> (pType t2) <+> pPos pos)
    t1 t2


undeclaredVariable :: Ident -> PosInfo -> TypeError
undeclaredVariable (Ident name) pos = UndeclaredVariable $
    "TypeError: undeclared variable" <+> name <+> pPos pos


multipleDeclarations :: Ident -> PosInfo -> TypeError
multipleDeclarations (Ident name) pos = MultipleDeclarations $
    "TypeError: multiple declarations of variable" <+> name <+> pPos pos


noReturn :: Ident -> PosInfo -> TypeError
noReturn (Ident name) pos = NoReturn $
    "TypeError: control may reach end of non-void function" <+> name ++ "declared at" <+> pPos pos


otherError :: Maybe String -> PosInfo -> TypeError
otherError mMsg pos = OtherError $ case mMsg of
    Nothing -> "TypeError occured" <+> pPos pos
    Just msg -> "TypeError:" <+> msg <+> pPos pos
