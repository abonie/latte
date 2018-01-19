{-# LANGUAGE FlexibleInstances #-}

module Semantics.SemanticMonad where

import qualified Data.Map as Map
import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.State
import Data.Maybe (fromJust, isJust)
import Parsing.AbsLatte
import Errors.LatteError
import Utils (lookupNested)
import Semantics.TypeError (typeMismatch, undeclaredVariable, multipleDeclarations, otherError)


type SymTable = Map.Map Ident PType

-- Maps class name to name of the super class
type TypeEnv = Map.Map Ident (Maybe Ident, [(Ident, PType)])

data Env
    = Env {
      retType :: Maybe PType
    , clsName :: Maybe Ident
    , symTables :: [SymTable]
    , typeEnv :: TypeEnv
    }

insert :: Ident -> PType -> Env -> Env
insert k v env = env { symTables = tabs } where
    hd:tl = symTables env
    tabs = (Map.insert k v hd):tl

emptyEnv :: Env
emptyEnv
    = Env {
      retType = Nothing
    , clsName = Nothing
    , symTables = [Map.empty]
    , typeEnv = Map.empty
    }


class Monad m => MonadSemanticCheck m where
    raise :: LatteError PType -> m b
    typeof :: Ident -> PosInfo -> m PType
    memberType :: Ident -> Ident -> PosInfo -> m PType
    declare :: PType -> Ident -> PosInfo -> m ()
    addClass :: Ident -> Maybe Ident -> PosInfo -> m ()
    checkClass :: Ident -> PosInfo -> m ()
    matchTypes :: PType -> PType -> PosInfo -> m PType
    returnType :: m PType
    enterClass :: Ident -> m ()
    leaveClass :: m ()
    enterBlock :: m ()
    leaveBlock :: m ()
    enterFunction :: PType -> m ()
    leaveFunction :: m ()
    runTypeCheck :: m a -> Either (LatteError PType) (a, TypeEnv)


type TCheck = ExceptT (LatteError PType) (State Env)


instance MonadSemanticCheck TCheck where
    raise = throwError

    matchTypes t1 t2 pos = do
        let t1' = rmpos t1
        let t2' = rmpos t2
        when (t1' /= t2') (throwError $ typeMismatch t1 t2 pos)
        return t1

    memberType cls mem pos = do
        env <- gets $ Map.lookup cls . typeEnv
        case env of
            Nothing -> throwError $ otherError (Just $ "undeclared class " ++ (show cls)) pos  -- XXX
            Just (_, members) -> do
                let typ = lookup mem members
                case typ of
                    Nothing -> throwError $ undeclaredVariable mem pos  -- XXX
                    Just t -> return t

    typeof var pos = do
        tabs <- gets symTables
        case lookupNested var tabs of
            Nothing -> throwError $ undeclaredVariable var pos
            Just t -> return t

    declare typ var pos = do
        when (rmpos typ == pVoid) (throwError $ otherError (Just "illegal variable type: void") pos)
        cls <- gets clsName
        case cls of
            Nothing -> do
                tab <- gets $ head . symTables
                when (Map.member var tab) (throwError $ multipleDeclarations var pos)
                modify $ insert var typ
            Just cname -> do
                tenv <- gets typeEnv
                let Just (super, members) = Map.lookup cname tenv
                when (member var members) (throwError $ multipleDeclarations var pos)
                modify $ \s -> s { typeEnv = Map.insert cname (super, (var, typ):members) tenv }
      where
        member x = any ((==x).fst)

    addClass ident super pos = do
        tenv <- gets typeEnv
        when (Map.member ident tenv) (throwError $ multipleDeclarations ident pos)
        -- TODO XXX check if super exists and there is no cycle
        modify $ \s -> s { typeEnv = Map.insert ident (super, []) tenv }

    checkClass ident pos = do
        tenv <- gets $ Map.lookup ident . typeEnv
        case tenv of
            Nothing -> throwError $ otherError (Just "undeclared class") pos -- XXX
            Just _ -> return ()

    enterClass ident = do
        modify $ \s -> s { clsName = Just ident }

    leaveClass = do
        modify $ \s -> s { clsName = Nothing }

    enterBlock = do
        tabs <- gets symTables
        modify $ \s -> s { symTables = Map.empty:tabs }

    leaveBlock = do
        (_:t) <- gets symTables
        modify $ \s -> s { symTables = t }

    returnType = gets $ fromJust . retType

    enterFunction typ = do
        enterBlock
        modify $ \s -> s { retType = (Just typ) }

    leaveFunction = do
        leaveBlock
        modify $ \s -> s { retType = Nothing }

    runTypeCheck tc = let (ast, env) = runState (runExceptT tc) emptyEnv in
        ast >>= \x -> return (x, typeEnv env)
