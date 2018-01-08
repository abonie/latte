{-# LANGUAGE FlexibleInstances #-}

module Compile.Monad where
import qualified LLVM
import qualified Data.Map as Map
import Data.Maybe (fromJust)
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
    startPhi :: m ()
    endPhi :: LLVM.Ident -> LLVM.Ident -> m ()
    currentLabel :: m LLVM.Ident
    runGen :: m () -> Either (LatteError PType) LLVM.Module


type LLGen = ExceptT (LatteError PType) (State Env)


data Env
    = Env {
      vars :: VarEnv
    , signs :: FunEnv
    , code :: Code
    , decls :: [LLVM.TopDef]
    , scope :: Ident
    , count :: Integer
    , phis :: [Map.Map Ident (LLVM.Operand, LLVM.Operand)]
    , label :: LLVM.Ident
    }

type VarEnv = Map.Map Ident (Maybe LLVM.Operand) -- TODO type?

type FunEnv = Map.Map Ident (Type (), [Arg ()])

type Code = Map.Map Ident [LLVM.Instr]

emptyEnv = Env {
            vars = Map.empty
          , signs = Map.empty
          , code = Map.empty
          , decls = []
          , scope = Ident ""
          , count = 0
          , phis = []
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
        vs <- gets vars
        modify $ \s -> s { vars = Map.insert ident Nothing vs }

    setVar ident val = do
        vs <- gets vars
        p <- gets phis
        unless (null p) $ do
            let h = head p
            let newh = if Map.member ident h
                        then Map.insertWith (\(old, _) (_, new) -> (old, new)) ident (val, val) h
                        else let old = fromJust (vs Map.! ident) in Map.insert ident (old, val) h
            modify $ \s -> s { phis = newh:(tail p) }
        modify $ \s -> s { vars = Map.insert ident (Just val) vs }

    getVar ident = do
        vs <- gets vars
        unless (Map.member ident vs) (throwError CompileError)  -- TODO
        let val = vs Map.! ident
        when (val == Nothing) (throwError CompileError)  -- TODO
        return $ fromJust val

    beginScope = return ()  -- TODO

    endScope = return ()  -- TODO

    beginFunction typ fname args = do
        fenv <- gets signs
        let fenv' = Map.insert fname (() <$ typ, map (() <$) args) fenv
        modify $ \s -> s { signs = fenv' }
        modify $ \s -> s { scope = fname }

    endFunction = do
        modify $ \s -> s { scope = Ident "" }

    startPhi = do
        p <- gets phis
        modify $ \s -> s { phis = Map.empty:p }

    endPhi l1 l2 = do
        (h:t) <- gets phis
        forM_ (Map.assocs h) (\(ident, (old, new)) -> do
            r <- newReg
            emit $ LLVM.Phi r LLVM.I32 old l1 new l2
            setVar ident (LLVM.Reg r) )
        modify $ \s -> s { phis = t }

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
