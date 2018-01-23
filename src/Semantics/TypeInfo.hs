{-# LANGUAGE FlexibleInstances #-}

module Semantics.TypeInfo where
import Parsing.AbsLatte


type TypeInfo = Maybe PType

mapnovars :: Functor f => f a -> f (a, TypeInfo)
mapnovars = fmap $ flip (,) Nothing

settype :: Functor f => PType -> f a -> f (a, TypeInfo)
settype t = fmap $ flip (,) (Just t)


class Typeable a where
    typeOf :: a -> PType


instance Typeable TypeInfo where
    typeOf (Just t) = t


instance Typeable (a, TypeInfo) where
    typeOf (_, ti) = typeOf ti


instance Typeable t => Typeable (Expr t) where
    typeOf expr = case expr of
        EVar t _ -> typeOf t
        EMem t _ _ -> typeOf t
        EInd t _ _ -> typeOf t
        ELitInt _ _ -> pInt
        ELitTrue _ -> pBool
        ELitFalse _ -> pBool
        ENew t _ -> typeOf t
        ENull _ ident -> TCls nopos ident
        EArr _ typ _ -> Arr nopos $ rmpos typ
        EApp t _ _ -> typeOf t
        EString _ _ -> pStr
        Neg _ _ -> pInt
        Not _ _ -> pBool
        EMul _ _ _ _ -> pInt
        EAdd t _ _ _ -> typeOf t
        ERel _ _ _ _ -> pBool
        EAnd _ _ _ -> pBool
        EOr _ _ _ -> pBool
