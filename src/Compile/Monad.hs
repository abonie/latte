{-# LANGUAGE FlexibleInstances #-}

module Compile.Monad where
import qualified LLVM
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad (unless)
import Control.Monad.Except
import Control.Monad.State
import Utils (lookupNested)
import Errors.LatteError
import Parsing.AbsLatte
import Semantics.TypeInfo


class Monad m => MonadCodeGen m where
    emit :: LLVM.Instr -> m ()
    newLabel :: m LLVM.Ident
    newReg :: m LLVM.Ident
    addDecl :: LLVM.TopDef -> m ()
    getNamedType :: LLVM.Ident -> m LLVM.Type
    addStr :: String -> LLVM.Ident -> m LLVM.Operand
    callStrlen :: LLVM.Operand -> m LLVM.Operand
    addVar :: Type (PosInfo, TypeInfo) -> Ident -> PosInfo -> m ()
    setVar :: Ident -> LLVM.Operand -> m ()
    getVar :: Ident -> m LLVM.Operand
    getVarType :: Ident -> m (Type (PosInfo, TypeInfo))
    newScope :: m ()
    endScope :: m ()
    beginFunction :: Type (PosInfo, TypeInfo) -> Ident -> [Arg (PosInfo, TypeInfo)] -> m ()
    endFunction :: m ()
    setLabel :: LLVM.Ident -> m ()
    currentLabel :: m LLVM.Ident
    convertType :: Type a -> m LLVM.Type
    runGen :: TypeEnv -> m () -> Either (LatteError PType) LLVM.Module


type LLGen = ExceptT (LatteError PType) (State Env)


--data BlockState
--    = BlockState {
--      label :: LLVM.Ident
--    , instrs :: [LLVM.Instr]
--    , term :: Maybe LLVM.Instr
--    }


data Env
    = Env {
      vars :: [VarEnv]
    , signs :: FunEnv
    , code :: Code
    , decls :: [LLVM.TopDef]
    , scope :: Ident
    , count :: Integer
    , label :: LLVM.Ident
    , typeEnv :: TypeEnv
    }

-- XXX copied over
type TypeEnv = Map.Map Ident (Maybe Ident, [(Ident, PType)])

type VarEnv = Map.Map Ident (LLVM.Operand, Type (PosInfo, TypeInfo))

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
    , typeEnv = Map.empty
    }

newEnv :: TypeEnv -> Env
newEnv te = emptyEnv { typeEnv = te }


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

    getNamedType name = do
        declarations <- gets decls
        let find elem = case elem of {
            LLVM.TypeDef ident _ | ident == name -> True ;
            _ -> False }
        let [LLVM.TypeDef _ typ] = filter find declarations
        return typ

    addStr str r = do
        n <- gets count
        modify $ \s -> s { count = n + 1 }
        let globname = LLVM.Ident $ "str" ++ (show n)
        let atype = LLVM.Array (max 1 $ (length str) - 1) LLVM.i8
        addDecl $ LLVM.ConstDef globname atype (LLVM.Str str)
        return $ LLVM.ConstOperand $ LLVM.Global (LLVM.Ptr atype) globname

    addVar typ ident _ = do  -- TODO blank: _
        r <- newReg
        typ' <- convertType typ
        emit $ LLVM.Alloc r typ'
        (vs:tl) <- gets vars
        modify $ \s -> s { vars = (Map.insert ident ((LLVM.Reg typ' r), typ) vs):tl }

    setVar ident val = do
        vs <- gets vars
        case lookupNested ident vs of
            Nothing -> throwError CompileError -- TODO
            Just (reg, _) -> emit $ LLVM.Store (LLVM.operandType reg) val reg

    callStrlen reg = do
        res <- newReg
        emit $ LLVM.Call res LLVM.i64 (LLVM.Ident "@strlen") [LLVM.Carg (LLVM.Ptr LLVM.i8) reg]
        return $ LLVM.Reg LLVM.i64 res

    getVar ident = do
        vs <- gets vars
        case lookupNested ident vs of
            Nothing -> throwError CompileError -- TODO
            Just (reg@(LLVM.Reg typ ident), _) -> do
                res <- newReg
                emit $ LLVM.Load res typ reg
                return $ LLVM.Reg typ res

    getVarType ident = do
        vs <- gets vars
        case lookupNested ident vs of
            Nothing -> throwError CompileError -- TODO
            Just (_, typ) -> return typ

    newScope = do
        vs <- gets vars
        modify $ \s -> s { vars = Map.empty:vs }

    endScope = do
        (_:vs) <- gets vars
        modify $ \s -> s { vars = vs }

    beginFunction typ fname args = do
        modify $ \s -> s { label = LLVM.Ident "%0" }
        fenv <- gets signs
        let fenv' = Map.insert fname (() <$ typ, map (() <$) args) fenv
        newScope
        modify $ \s -> s { scope = fname }
        forM_ args (\(Arg (pos,_) typ ident@(Ident name)) -> do
            addVar typ ident pos
            typ' <- convertType typ
            setVar ident (LLVM.Reg typ' $ LLVM.Ident $ '%':name) )
        modify $ \s -> s { signs = fenv' }

    endFunction = do
        fenv <- gets signs
        fident <- gets scope
        cd <- gets code
        let Just (retType, _) = Map.lookup fident fenv
        when (retType == Void ()) $ do
            -- TODO XXX
            case Map.lookup fident cd of
                Nothing -> emit LLVM.VRet
                Just [] -> emit LLVM.VRet
                Just (i:is) | i /= LLVM.VRet -> emit LLVM.VRet
                _ -> return ()
        cd <- gets code
        let cleaned = rmlabels $ cd Map.! fident  -- TODO XXX
        modify $ \s -> s { scope = Ident "", code = Map.insert fident cleaned cd }
        endScope
      where
        -- TODO XXX
        rmlabels ((LLVM.Label _):xs) = rmlabels xs
        rmlabels other = other

    setLabel lab = do
        emit $ LLVM.Label lab
        modify $ \s -> s { label = lab }

    currentLabel = do
        l <- gets label
        return l

    -- TODO redundant
    convertType typ = return $ typeToLLVM typ

    runGen te llgen = let (res, state) = runState (runExceptT llgen) (newEnv te) in
        either Left (const $ Right $ envToModule state) res


envToModule :: Env -> LLVM.Module
envToModule env = let
    mkFunDef (ident@(Ident name), instrs) = let
            Just (ret, args) = Map.lookup ident (signs env)
            args' = map argToLLVM args in
        LLVM.FunDef (typeToLLVM ret) (LLVM.Ident ('@':name)) args' (reverse instrs) in
    LLVM.Module $ (decls env) ++ map mkFunDef (Map.assocs $ code env)


typeToLLVM :: Type a -> LLVM.Type
typeToLLVM (Int _) = LLVM.i64
typeToLLVM (Bool _) = LLVM.i1
typeToLLVM (Void _) = LLVM.Void
typeToLLVM (Str _) = LLVM.Ptr LLVM.i8
typeToLLVM (Arr _ t) = LLVM.Struct [LLVM.Ptr $ typeToLLVM t, LLVM.i64]
typeToLLVM (TCls _ (Ident name)) =
    LLVM.Ptr $ LLVM.NamedType $ LLVM.Ident $ "%struct." ++ name

argToLLVM :: Arg a -> LLVM.Arg
argToLLVM (Arg _ typ (Ident name)) = LLVM.Arg (typeToLLVM typ) (LLVM.Ident $ '%':name)
