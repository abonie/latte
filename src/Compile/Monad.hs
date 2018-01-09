{-# LANGUAGE FlexibleInstances #-}

module Compile.Monad where
import qualified LLVM
import qualified Data.Map as Map
import qualified Data.Set as Set
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
    beginScope :: m ()
    endScope :: m ()
    beginFunction :: Type (PosInfo, TypeInfo) -> Ident -> [Arg (PosInfo, TypeInfo)] -> m ()
    endFunction :: m ()
    beginBasicBlock :: LLVM.Ident -> m ()
    endBasicBlock :: m ((Map.Map Ident (LLVM.Operand, LLVM.Operand)), LLVM.Ident)
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
    , used :: [Set.Set Ident]
    , label :: LLVM.Ident
    }

type VarEnv = Map.Map Ident LLVM.Operand

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
    , used = []
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

    addVar _ ident _ = do
        (vs:tl) <- gets vars
        modify $ \s -> s { vars = (Map.insert ident LLVM.Undef vs):tl }

    setVar ident val = do
        (vs:tl) <- gets vars
        us <- gets used
        unless (null us) $ do
            let (u:tl) = us
            modify $ \s -> s { used = (Set.insert ident u):tl }
        modify $ \s -> s { vars = (Map.insert ident val vs):tl }

    getVar ident = do
        (vs:_) <- gets vars
        unless (Map.member ident vs) (throwError CompileError)  -- TODO
        let val = vs Map.! ident
        --when (val == LLVM.Undef) (throwError CompileError)  -- TODO
        return val

    beginScope = return ()  -- TODO

    endScope = return ()  -- TODO

    beginFunction typ fname args = do
        fenv <- gets signs
        let fenv' = Map.insert fname (() <$ typ, map (() <$) args) fenv
        modify $ \s -> s { signs = fenv' }
        modify $ \s -> s { scope = fname }

    endFunction = do
        modify $ \s -> s { scope = Ident "" }

    setLabel lab = do
        emit $ LLVM.Label lab
        modify $ \s -> s { label = lab }

    beginBasicBlock lab = do
        setLabel lab
        us <- gets used
        modify $ \s -> s { used = Set.empty:us }
        (vs:tl) <- gets vars
        modify $ \s -> s { vars = vs:vs:tl }

    endBasicBlock = do
        (u:us) <- gets used
        modify $ \s -> s { used = us }
        (vs:tl) <- gets vars
        modify $ \s -> s { vars = tl }
        let prev = head tl
        lab <- currentLabel
        return (Map.fromList $ map (\ident -> (ident, (prev Map.! ident, vs Map.! ident))) $ filter (flip Map.member prev) $ Set.toList u, lab)

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
