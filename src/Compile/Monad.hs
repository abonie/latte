{-# LANGUAGE FlexibleInstances #-}

module Compile.Monad where
import qualified LLVM
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (isJust)
import Control.Monad (unless)
import Control.Monad.Except
import Control.Monad.State
import Errors.LatteError
import Parsing.AbsLatte
import Semantics.TypeCheck (TypeInfo(..))


class Monad m => MonadCodeGen m where
    emit :: LLVM.Instr -> m ()
    newLabel :: m LLVM.Ident
    newReg :: m LLVM.Ident
    addDecl :: LLVM.TopDef -> m ()
    addStr :: String -> m LLVM.Ident
    addVar :: Type (PosInfo, TypeInfo) -> Ident -> PosInfo -> m ()
    setVar :: Ident -> LLVM.Operand -> m ()
    getVar :: Ident -> m LLVM.Operand
    newScope :: m ()
    endScope :: m ()
    beginFunction :: Type (PosInfo, TypeInfo) -> Ident -> [Arg (PosInfo, TypeInfo)] -> m ()
    endFunction :: m ()
    setLabel :: LLVM.Ident -> m ()
    currentLabel :: m LLVM.Ident
    runGen :: m () -> Either (LatteError PType) LLVM.Module


type LLGen = ExceptT (LatteError PType) (State Env)


data Env
    = Env {
      vars :: [VarEnv]
    , signs :: FunEnv
    , code :: Code
    , decls :: [LLVM.TopDef]
    , scope :: Ident
    , count :: Integer
    , label :: LLVM.Ident
    }

type VarEnv = Map.Map Ident (LLVM.Operand, LLVM.Type)

lookupNested :: Ord k => k -> [Map.Map k a] -> Maybe a
lookupNested key = foldl foo Nothing where
    foo acc map = case acc of
        Nothing -> Map.lookup key map
        _ -> acc

memberNested :: Ord k => k -> [Map.Map k a] -> Bool
memberNested key = isJust . lookupNested key


type FunEnv = Map.Map Ident (Type (), [Arg ()])

type Code = Map.Map Ident [LLVM.Instr]

emptyEnv :: Env
emptyEnv
    = Env {
      vars = [Map.empty]
    , signs = Map.empty
    , code = Map.empty
    , decls = []
    , scope = Ident ""
    , count = 0
    , label = LLVM.Ident "%0"
    }


instance MonadCodeGen LLGen where
    emit instr = do
        cd <- gets code
        key <- gets scope
        modify $ \s -> s { code = Map.insertWith (++) key [instr] cd }

    newLabel = do
        n <- gets count
        modify $ \s -> s { count = n + 1 }
        return $ LLVM.Ident $ "%L" ++ (show n)

    newReg = do
        n <- gets count
        modify $ \s -> s { count = n + 1 }
        return $ LLVM.Ident $ "%t" ++ (show n)

    addDecl d = do
        declarations <- gets decls
        modify $ \s -> s { decls = d:declarations }

    addStr str = do
        n <- gets count
        modify $ \s -> s { count = n + 1 }
        let globname = LLVM.Ident $ "@str" ++ (show n)
        let atype = LLVM.Array ((length str) - 1) LLVM.I8
        addDecl $ LLVM.ConstDef globname atype (LLVM.LitStr str)
        return globname

    addVar typ ident _ = do  -- TODO blank: _
        r <- newReg
        let typ' = (typeToLLVM typ)
        emit $ LLVM.Alloc r typ'
        (vs:tl) <- gets vars
        modify $ \s -> s { vars = (Map.insert ident ((LLVM.Reg r), typ') vs):tl }

    setVar ident val = do
        vs <- gets vars
        case lookupNested ident vs of
            Nothing -> throwError CompileError -- TODO
            Just (reg, typ) -> emit $ LLVM.Store typ val reg

    getVar ident = do
        vs <- gets vars
        case lookupNested ident vs of
            Nothing -> throwError CompileError -- TODO
            Just (reg, typ) -> do
                res <- newReg
                emit $ LLVM.Load res typ reg
                return $ LLVM.Reg res

    newScope = do
        vs <- gets vars
        modify $ \s -> s { vars = Map.empty:vs }

    endScope = do
        (_:vs) <- gets vars
        modify $ \s -> s { vars = vs }

    beginFunction typ fname args = do
        fenv <- gets signs
        let fenv' = Map.insert fname (() <$ typ, map (() <$) args) fenv
        newScope
        forM_ args (\(Arg (pos,_) typ ident) -> addVar typ ident pos)
        modify $ \s -> s { signs = fenv' }
        modify $ \s -> s { scope = fname }

    endFunction = do
        modify $ \s -> s { scope = Ident "" }
        endScope

    setLabel lab = do
        emit $ LLVM.Label lab
        modify $ \s -> s { label = lab }

    currentLabel = do
        l <- gets label
        return l

    runGen llgen = let (res, state) = runState (runExceptT llgen) emptyEnv in
        either Left (const $ Right $ envToModule state) res


envToModule :: Env -> LLVM.Module
envToModule env = let
    mkFunDef (ident@(Ident name), instrs) = let
            (ret, args) = (signs env) Map.! ident
            args' = map argToLLVM args in
        LLVM.FunDef (typeToLLVM ret) (LLVM.Ident ('@':name)) args' (reverse instrs) in
    LLVM.Module $ (decls env) ++ map mkFunDef (Map.assocs $ code env)


typeToLLVM :: Type a -> LLVM.Type
typeToLLVM (Int _) = LLVM.I32
typeToLLVM (Bool _) = LLVM.I1
typeToLLVM (Void _) = LLVM.Void
typeToLLVM (Str _) = LLVM.Ptr LLVM.I8

argToLLVM :: Arg a -> LLVM.Arg
argToLLVM (Arg _ typ (Ident name)) = LLVM.Arg (typeToLLVM typ) (LLVM.Ident name)
