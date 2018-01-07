{-# Language DeriveFunctor #-}
{-# Language FlexibleInstances #-}

module Errors.LatteError where


data LatteError a = TErr (TypeError a)
                  | ParserError String
                  | CompileError
    deriving (Eq, Show)


data TypeError a = TypeMismatch String a a
               | UndeclaredVariable String
               | MultipleDeclarations String
               | NoReturn String
               | OtherError String
    deriving (Eq, Show)


--instance Show (TypeError a) where
--    show (TypeMismatch s _ _) = s
--    show (UndeclaredVariable s) = s
--    show (MultipleDeclarations s) = s
--    show (NoReturn s) = s
--    show (OtherError s) = s
