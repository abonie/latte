{-# LANGUAGE FlexibleInstances #-}

module SemanticMonad where

import qualified Data.Map as Map
import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.State
import Data.Maybe (fromJust)
import AbsLatte
import TypeError

-- TODO XXX: handling blocks by copying Env may be
--           inefficient in terms of memory usage
--
-- TODO: with this approach of copying Env for new
--       blocks, maintaining blockDepth variable
--       may be unnecessary
--

type SymTable = Map.Map Ident (PType, Int)

data Env = Env {
    blockDepth :: Int,
    retType :: Maybe PType,
    symTables :: [SymTable]
}

symTable :: Env -> SymTable
symTable = head . symTables

insert :: Ident -> (PType, Int) -> Env -> Env
insert k v (Env d r (tab:rest)) = Env d r $ (Map.insert k v tab):rest

emptyEnv :: Env
emptyEnv = Env 0 Nothing [Map.empty]


class Monad m => MonadSemanticCheck m where
    raise :: TypeError -> m a
    typeof :: Ident -> PosInfo -> m PType
    declare :: PType -> Ident -> PosInfo -> m ()
    matchTypes :: PType -> PType -> PosInfo -> m PType
    returnType :: m PType
    enterBlock :: m ()
    leaveBlock :: m ()
    enterFunction :: PType -> m ()
    leaveFunction :: m ()
    runTypeCheck :: m a -> Either TypeError SymTable


type TCheck = ExceptT TypeError (State Env)


instance MonadSemanticCheck TCheck where
    raise = throwError

    matchTypes t1 t2 pos = do
        let t1' = rmpos t1
        let t2' = rmpos t2
        when (t1' /= t2') (throwError $ typeMismatch t1 t2 pos)
        return t1

    typeof var pos = do
        maybeType <- gets $ (Map.lookup var) . symTable
        case maybeType of
            Nothing -> throwError $ undeclaredVariable var pos
            Just (t, _) -> return t

    declare typ var pos = do
        when (rmpos typ == pVoid) (throwError $ otherError (Just "illegal variable type: void") pos)
        tab <- gets symTable
        depth <- gets blockDepth
        when (Map.member var tab && snd (tab Map.! var) == depth)
             (throwError $ multipleDeclarations var pos)
        modify $ insert var (typ, depth)

    enterBlock = do
        (Env d r t) <- get
        put $ (Env (d+1) r ((head t):t))

    leaveBlock = do
        (Env d r t) <- get
        when (d == 0) (throwError $ otherError Nothing nopos) -- XXX
        put $ (Env (d-1) r (tail t))

    returnType = gets $ fromJust . retType

    enterFunction typ = do
        enterBlock
        (Env d _ t) <- get
        put $ Env d (Just typ) t

    leaveFunction = do
        leaveBlock
        (Env d _ t) <- get
        put $ Env d Nothing t

    runTypeCheck tc = let (val, state) = runState (runExceptT tc) emptyEnv in
        (symTable state) <$ val
