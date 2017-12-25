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

type TypeEnv = Map.Map Ident (Maybe Ident)

data Env = Env {
    blockDepth :: Int,
    retType :: Maybe PType,
    symTables :: [SymTable],
    typeEnv :: TypeEnv
}

symTable :: Env -> SymTable
symTable = head . symTables

insert :: Ident -> (PType, Int) -> Env -> Env
insert k v (Env d r (stab:rest) typ) = Env d r ((Map.insert k v stab):rest) typ

emptyEnv :: Env
emptyEnv = Env 0 Nothing [Map.empty] Map.empty


class Monad m => MonadSemanticCheck m where
    raise :: TypeError -> m a
    typeof :: Ident -> PosInfo -> m PType
    declare :: PType -> Ident -> PosInfo -> m ()
    addClass :: Ident -> Maybe Ident -> PosInfo -> m ()
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

    addClass ident super pos = do
        tenv <- gets typeEnv
        when (Map.member ident tenv) (throwError $ multipleDeclarations ident pos)
        -- TODO XXX check if super exists and there is no cycle
        modify $ \s -> s { typeEnv = Map.insert ident super tenv }

    enterBlock = do
        depth <- gets blockDepth
        (h:t) <- gets symTables
        modify $ \s -> s { blockDepth = depth+1, symTables = (h:h:t) }

    leaveBlock = do
        depth <- gets blockDepth
        (_:t) <- gets symTables
        when (depth == 0) (throwError $ otherError Nothing nopos) -- XXX
        modify $ \s -> s { blockDepth = depth-1, symTables = t }

    returnType = gets $ fromJust . retType

    enterFunction typ = do
        enterBlock
        modify $ \s -> s { retType = (Just typ) }

    leaveFunction = do
        leaveBlock
        modify $ \s -> s { retType = Nothing }

    runTypeCheck tc = let (val, state) = runState (runExceptT tc) emptyEnv in
        (symTable state) <$ val
