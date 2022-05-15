module Interpeter where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import System.IO
import qualified Data.Map as Map
import Data.Maybe

import AbsGrammar

type Loc = Int

data FlagEnv = FlagEnv {
                return_v :: Maybe Value,
                is_break :: Bool,
                is_continue :: Bool,
                main_fun_block :: Bool
              }

data Env = Env {
            _env :: Map.Map Ident Loc,
            flags :: FlagEnv
          }

type IM a = ExceptT String (StateT Store (ReaderT Env IO)) a

data FArg = FArg Value | FArgRef Loc

data Value = BoolV Bool | IntV Integer | StringV String | VoidV | FunV [Arg] ([FArg] -> IM Value) | TupleV [Value]

data Store = Store_ {
    memory :: Map.Map Loc Value,
    emptySlot :: Loc
}

valueToStr :: Value -> String

valueToStr (BoolV b) = show b

valueToStr (IntV v) = show v

valueToStr (StringV s) = s

createFlags :: FlagEnv

createFlags = FlagEnv {
                        return_v = Nothing,
                        is_break = False,
                        is_continue = False,
                        main_fun_block = False
                      }

createStore :: Store

createStore = Store_ { memory = Map.empty, emptySlot = 0 }

createEnv :: Env

createEnv = Env { _env = Map.empty, flags = createFlags }

runInterpreter :: Program -> IO (Either String Env, Store)

runInterpreter p = runReaderT (runStateT (runExceptT (evalProgram p)) createStore) createEnv

setReturnV :: Value -> Env -> Env

setReturnV v env = env { flags = (flags env) {return_v = Just v}}

resetReturnV :: Env -> Env

resetReturnV env = env { flags = (flags env) {return_v = Nothing}}

setBreak :: Env -> Env

setBreak env = env { flags = (flags env) {is_break = True}}

unSetBreak :: Env -> Env

unSetBreak env = env { flags = (flags env) {is_break = False}}

setContinue :: Env -> Env

setContinue env = env { flags = (flags env) {is_continue = True}}

unSetContinue :: Env -> Env

unSetContinue env = env {flags = (flags env) {is_continue = False}}

setMainBlock :: Env -> Env

setMainBlock env = env {flags = (flags env) {main_fun_block = True}}

unSetMainBlock :: Env -> Env

unSetMainBlock env = env {flags = (flags env) {main_fun_block = False}}

readFromMem :: Ident -> IM Value

readFromMem i = do
  env <- ask
  st  <- get
  let l = fromJust $ Map.lookup i $ _env env
  return $ fromJust $ Map.lookup l (memory st)

declareVar :: Ident -> Value -> IM Env

declareVar i v = do
  env <- ask
  st  <- get

  let loc = emptySlot st

  put (st { memory = Map.insert loc v $ memory st,
            emptySlot = (loc + 1) })

  return $ env { _env = Map.insert i loc $ _env env }



changeLocVal :: Loc -> Value -> IM ()

changeLocVal l v = do
  st <- get
  put (st { memory = Map.insert l v $ memory st })

assignVar :: Ident -> Value -> IM ()

assignVar i v = do
  env <- ask
  st <- get

  let loc = fromJust $ Map.lookup i $ _env env

  changeLocVal loc v


getDefaultValue :: Type -> Value

getDefaultValue (Int _) = IntV 0

getDefaultValue (Str _) = StringV ""

getDefaultValue (Bool _) = BoolV False

getDefaultValue (Void _) = VoidV

getDefaultValue (Tuple _ l) = TupleV $ fmap getDefaultValue l

getDefaultValue (Lambda pos _ _) = FunV [] (\l -> do throwError $ "Runtime error: Call to an uninitialized function or lambda at pos: " ++ (show pos))

declareVarDefault :: Ident -> Type -> IM Env

declareVarDefault i t = declareVar i $ getDefaultValue t

convertToFArg :: Arg -> Expr -> IM FArg

convertToFArg (Arg _ _ _) e = do
  env <- ask
  val <- evalExpr e
  return $ FArg val

convertToFArg (ArgRef _ _ _) (EVar _ i) = do
  env <- ask
  return $ FArgRef $ fromJust $ Map.lookup i $ _env env

evalExpr :: Expr -> IM Value

evalExpr (ETuple pos l) = do
  vals <- mapM evalExpr l
  return $ TupleV vals

-- #TODO
--evalExpr (ELambdaApp pos e l) = do
--  Lambda pos2 arg_l _ <- evalExpr e

--  return expression

evalExpr (ELambdaApp pos e l) = do
  FunV args f <- evalExpr e
  f_args      <- mapM (\(x, y) -> convertToFArg x y) $ zip args l
  f f_args

evalExpr (EVar _ i) = readFromMem i

evalExpr (ELitInt _ val) = return $ IntV val

evalExpr (ELitTrue _ ) = return $ BoolV True

evalExpr (ELitFalse _ ) = return $ BoolV False

evalExpr (EApp a i l) = do
  FunV args f <- readFromMem i
  f_args      <- mapM (\(x, y) -> convertToFArg x y) $ zip args l
  f f_args

evalExpr (EString _ s) = return $ StringV s

evalExpr (Neg _ e) = do
  IntV val <- evalExpr e
  return $ IntV $ (-1) * val

evalExpr (Not _ e) = do
  BoolV val <- evalExpr e
  return $ BoolV $ not val

evalExpr (EMul pos e1 op e2) = do
  IntV val1 <- evalExpr e1
  IntV val2 <- evalExpr e2
  case op of
    Times _ -> return $ IntV $ val1 * val2
    Div _ -> do
      when (val2 == 0)
          (throwError $ "Division by 0 at position: " ++ (show pos))
      return $ IntV $ val1 `div` val2
    Mod _ -> do
      when (val2 == 0)
        (throwError $ "Modular division by 0 at position: " ++ (show pos))
      return $ IntV $ val1 `mod` val2

evalExpr (EAdd _ e1 op e2) = do
  IntV val1 <- evalExpr e1
  IntV val2 <- evalExpr e2

  case op of
    Plus _  -> return $ IntV $ val1 + val2
    Minus _ -> return $ IntV $ val1 - val2

evalExpr (ERel _ e1 op e2) = do
  IntV val1 <- evalExpr e1
  IntV val2 <- evalExpr e2

  case op of
    LTH _ -> return $ BoolV $ val1 < val2
    LE _  -> return $ BoolV $ val1 <= val2
    GTH _ -> return $ BoolV $ val1 > val2
    GE _  -> return $ BoolV $ val1 >= val2
    EQU _ -> return $ BoolV $ val1 == val2
    NE _  -> return $ BoolV $ val1 /= val2

evalExpr (EAnd _ e1 e2) = do
  BoolV val1 <- evalExpr e1
  BoolV val2 <- evalExpr e2

  return $ BoolV $ val1 && val2

evalExpr (EOr _ e1 e2) = do
  BoolV val1 <- evalExpr e1
  BoolV val2 <- evalExpr e2

  return $ BoolV $ val1 || val2

evalExpr (ELambda _ args ret_t b) = do
  env <- ask
  return $ FunV args (\l -> local (const env) $ do
                                    inner_env <- declareFunArgs args l
                                    inner_env2 <- local (const inner_env) $ evalBlock b
                                    return $ fromJust $ return_v $ flags inner_env2)

evalProgram :: Program -> IM Env

evalProgram (Program p []) = do
  v <- evalExpr (EApp Nothing (Ident "main") [])
  ask

evalProgram (Program p (x:xs)) = do
  env <- declareFun x
  local (const env) (evalProgram (Program p xs))

evalBlock :: Block -> IM Env

evalBlock (Block _ l) = do
  env  <- ask
  env2 <- evalStmts l
  return $ env {flags = flags env2}
  where
    evalStmts :: [Stmt] -> IM Env
    evalStmts [] = ask
    evalStmts (x:xs) = do
      env <- evalStmt x
      let fs = flags env
      if ((is_continue fs) || (is_break fs) || (isJust $ return_v fs)) then
        return env
      else
        local (const env) (evalStmts xs)

evalStmt :: Stmt -> IM Env

evalStmt (Empty _) = do
  env <- ask
  return env

evalStmt (BStmt _ b) = do
  env <- ask
  env2 <- evalBlock b
  return $ env {flags = flags env2}

evalStmt (Decl _ _ []) = ask

evalStmt (Decl p t (x:xs)) = case x of
  NoInit _ i -> do
    env <- declareVarDefault i t
    local (const env) $ evalStmt (Decl p t xs)
  Init _ i e -> do
    val <- evalExpr e
    env <- declareVar i val
    local (const env) $ evalStmt (Decl p t xs)

evalStmt (InnerFun _ f) = declareFun f

evalStmt (Ass _ i e) = do
  env <- ask
  val <- evalExpr e
  assignVar i val
  return env

evalStmt (TAss _ l e) = do
  env <- ask
  TupleV tl <- evalExpr e

  let idents = fmap extractIdent l

  mapM (\(i, t_v) -> assignVar i t_v) $ zip idents tl

  return env

evalStmt (Incr _ i) = do
  env <- ask
  IntV v <- readFromMem i
  assignVar i $ IntV (v + 1)
  return env

evalStmt (Decr _ i) = do
  env <- ask
  IntV v <- readFromMem i
  assignVar i $ IntV (v - 1)
  return env

evalStmt (Ret _ e) = do
  env <- ask
  val <- evalExpr e

  return $ setReturnV val env

evalStmt (VRet _) = do
  env <- ask

  return $ setReturnV VoidV env

evalStmt (Cond _ e b) = do
  env <- ask
  BoolV b_val <- evalExpr e
  if (b_val) then
    do
    env2 <- evalBlock b
    return $ env {flags = flags env2}
  else
    return env

evalStmt (CondElif p e b [] else_b) = do
  env <- ask
  BoolV b_val <- evalExpr e
  if (b_val) then
    do
    env2 <- evalBlock b
    return $ env {flags = flags env2}
  else
    do
    env2 <- evalBlock else_b
    return $ env {flags = flags env2}

evalStmt (CondElif p e b ((Elif _ elif_e elif_b):xs) else_b) = do
  env <- ask
  BoolV b_val <- evalExpr e
  if (b_val) then
    do
    env2 <- evalBlock b
    return $ env {flags = flags env2}
  else
    do
    local (const env) (evalStmt $ CondElif p elif_e elif_b xs else_b)

evalStmt stmt@(While _ e s) = do
  env <- ask
  BoolV b_val <- evalExpr e
  if (b_val) then
    do
    env2 <- evalStmt s
    let fs = flags env2
    if (is_continue fs) then
      do
      local (const env) (evalStmt stmt)
    else if (is_break fs) then
      return env
      else
        case (return_v fs) of
          Just v -> return $ env {flags = flags env2}
          Nothing -> do local (const env) (evalStmt stmt)
  else
    return env

evalStmt (For pos t i i_e cond_e stmt_after loop_stmt) = do
  val <- evalExpr i_e
  env <- declareVar i val
  local (const env) (forLoop cond_e stmt_after loop_stmt)
  ask
  where
    forLoop :: Expr -> Stmt -> Stmt -> IM Env
    forLoop cond_e stmt_after loop_stmt = do
      BoolV b_val <- evalExpr cond_e
      if (b_val) then
        do
        after_loop_env <- evalStmt loop_stmt
        let fs = flags after_loop_env
        if (is_continue fs) then
          do
          evalStmt stmt_after
          forLoop cond_e stmt_after loop_stmt
        else if (is_break fs) then ask
          else do
            env <- ask
            case (return_v fs) of
              Just v -> return $ env {flags = flags after_loop_env}
              Nothing -> do
                local (const env) (evalStmt stmt_after)
                forLoop cond_e stmt_after loop_stmt
      else
        ask


evalStmt (Break _) = do
  env <- ask
  return $ setBreak env

evalStmt (Continue _) = do
  env <- ask
  return $ setContinue env

evalStmt (SExp _ e) = do
  env <- ask
  evalExpr e
  return env

evalStmt (Print _ l) = do
  env <- ask
  expr_l <- mapM evalExpr l
  let s_l = fmap valueToStr expr_l
  liftIO $ putStr $ unwords  $ s_l ++ ["\n"]
  return env

extractIdent :: TItem -> Ident

extractIdent (TItem _ i) = i

declareFunArgs :: [Arg] -> [FArg] -> IM Env

declareFunArgs [] [] = do
  env <- ask
  return env

declareFunArgs ((Arg _ _ i):t_arg) ((FArg val):t_farg) = do
  env <- declareVar i val
  local (const $ env) (declareFunArgs t_arg t_farg)

declareFunArgs ((ArgRef _ _ i):t_arg) ((FArgRef loc):t_farg) = do
  env <- ask
  local (const $ env {_env = (Map.insert i loc $ _env env)}) (declareFunArgs t_arg t_farg)

declareFun :: TopDef -> IM Env

declareFun (FnDef pos ret_t i args b) = do
  env <- ask
  st <- get
  let nextLoc = emptySlot st
  let env2 = env {_env = Map.insert i nextLoc $ _env env}
  declareVar i $ FunV args (\l -> local (const env2) $ do
                                    inner_env <- declareFunArgs args l
                                    inner_env2 <- local (const inner_env) $ evalBlock b
                                    return $ fromJust $ return_v $ flags inner_env2)
