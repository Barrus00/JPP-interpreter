module TypeChecker (runChecker, checkProgram) where

import Prelude

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad (when)

import Data.Maybe
import qualified Data.Map as Map
import Data.Data (toConstr)

import AbsGrammar

type Name = String

data RefType = Ref Type | NotRef Type

data Env = Env { _envTypes :: Map.Map Ident (Type, Integer),
                 _envCurrLvl :: Integer,
                 _envReturnType :: Maybe Type }

createEnv :: Env
createEnv = Env { _envTypes = Map.empty,
                  _envCurrLvl = 0,
                  _envReturnType = Nothing }

type CheckerT a = ExceptT String (Reader Env) a

printConstr :: (Show a) => a -> String

printConstr = head . words . show

runChecker :: Program -> Either String Env

runChecker p = runReader (runExceptT (checkProgram p)) createEnv

checkTypeEq :: Type -> Type -> Bool

checkTypeEq (Tuple _ typeList1) (Tuple _ typeList2) = checkTypeLists typeList1 typeList2

checkTypeEq (Lambda _ argList1 r1) (Lambda _ argList2 r2) = if (checkTypeEq r1 r2)
  then checkArgLists argList1 argList2
  else False

checkTypeEq (Fun _ r1 argList1) (Fun _ r2 argList2) = if (checkTypeEq r1 r2)
  then checkArgLists argList1 argList2
  else False

checkTypeEq t1 t2 = (printConstr t1) == (printConstr t2)

checkArgLists :: [Arg] -> [Arg] -> Bool

checkArgLists l1 l2 = and $ fmap (\(x, y) -> checkArgEq x y) $ zip l1 l2 where
  checkArgEq :: Arg -> Arg -> Bool
  checkArgEq (Arg _ t1 i1) (Arg _ t2 i2) = (checkTypeEq t1 t2) && (i1 == i2)
  checkArgEq (ArgRef _ t1 i1) (ArgRef _ t2 i2) = (checkTypeEq t1 t2) && (i1 == i2)
  checkArgEq _ _ = False

checkTypeLists :: [Type] -> [Type] -> Bool

checkTypeLists l1 l2 = and $ fmap (\(x, y) -> checkTypeEq x y) $ zip l1 l2

assertType :: Type -> Expr -> CheckerT Type

assertType t expr = do
  expr_t <- evalType expr
  if (not $ checkTypeEq t expr_t) then
    throwError $ "Mismatched type at position: " ++ (show $ hasPosition expr)
  else
    return expr_t


evalType :: Expr -> CheckerT Type

evalType (ELambda pos args ret_t block) = do
  env <- ask
  new_env <- local (const env) (declareArgs args)
  local (const $ incLevel $ setReturnType new_env ret_t) (checkBlock block)
  return (Lambda pos args ret_t)

evalType (EOr pos e1 e2) = do
  assertType (Bool Nothing) (e1)
  assertType (Bool Nothing) (e2)
  return (Bool pos)

evalType (EAnd pos e1 e2) = do
  assertType (Bool Nothing) (e1)
  assertType (Bool Nothing) (e2)
  return (Bool pos)

evalType (ERel pos e1 op e2) = do
  assertType (Int Nothing) e1
  assertType (Int Nothing) e2
  return (Bool pos)

evalType (EAdd pos e1 op e2) = do
  assertType (Int Nothing) e1
  assertType (Int Nothing) e2
  return (Int pos)

evalType (EMul pos e1 op e2) = do
  assertType (Int Nothing) e1
  assertType (Int Nothing) e2
  return (Int pos)

evalType (Not pos e1) = do
  assertType (Bool Nothing) e1
  return (Bool pos)

evalType (Neg pos e1) = do
  assertType (Int Nothing) e1
  return (Int pos)

evalType (EString pos _) = return (Str pos)

evalType (EApp pos f args) = do
  env <- ask
  case (Map.lookup f (_envTypes env)) of
    Nothing -> throwError $ "Unbound function " ++ (show f) ++ " at position: " ++ (show pos)
    Just (Fun pos retType argsType, _) -> do
      isOk <- checkFunArgs argsType args
      case isOk of
        Just err -> throwError $ "Error when calling a function on position: " ++ (show pos) ++ err
        Nothing -> return retType
    _ -> throwError $ (show f) ++ " is not a function, it can't be called. " ++ (show pos)

evalType (ELitFalse pos) = return (Bool pos)

evalType (ELitTrue pos) = return (Bool pos)

evalType (ELitInt pos _) = return (Int pos)

evalType (EVar pos x) = do
  env <- ask
  case Map.lookup x $ _envTypes env of
    Nothing -> throwError $ "Unbound variable " ++ (show x) ++ " at position: " ++ (show pos)
    Just (t, _) -> return t

evalType (ELambdaApp pos e1 args) = do
  t <- evalType e1
  case t of
    Lambda pos2 argsType retType -> do
      isOk <- checkFunArgs argsType args
      case isOk of
        Just err -> throwError $ "Error when calling a lambda on position: " ++ (show pos) ++ err
        Nothing -> return retType
    t -> throwError $ "Not a lambda at position: " ++ (show pos)

evalType (ETuple pos el) = do
  typeList <- mapM evalType el
  return $ Tuple pos typeList

incLevel :: Env -> Env

incLevel env = env {_envCurrLvl = (_envCurrLvl env) + 1}

setReturnType :: Env -> Type -> Env

setReturnType env t = env {_envReturnType = Just t}

declareType :: Ident -> Type -> CheckerT Env

declareType i t = do
  env <- ask
  let env2 = env {_envTypes = Map.insert i (t, _envCurrLvl env) (_envTypes env)}
  case (Map.lookup i $ _envTypes env) of
    Nothing -> return env2
    Just (t, lvl) ->
      if (lvl == (_envCurrLvl env)) then
        throwError $ "Variable " ++ (show i) ++ "defined previously at " ++ (show $ hasPosition t)
      else
        return env2

checkLValue :: Expr -> Bool

checkLValue (EVar pos i) = True

checkLValue _ = False

checkFunArgs :: [Arg] -> [Expr] -> CheckerT (Maybe String)

checkFunArgs [] [] = return Nothing

checkFunArgs [] _ = return $ Just "Too many arguments to call a function."

checkFunArgs _ [] = return $ Just "Not enough arguments to call a function."

checkFunArgs (h_args:t_args) (h_exp:t_exp) = do
  exp_t <- evalType h_exp
  case h_args of
    Arg pos t _->
      if (not $ checkTypeEq t exp_t) then
        return $ Just ((show pos) ++ " - Mismatched argument type, expected: " ++ (show t) ++ " but got: " ++ (show exp_t))
      else
        checkFunArgs t_args t_exp
    ArgRef pos t _ ->
      if (not $ checkLValue h_exp) then
        return $ Just $ (show pos) ++  " - Lvalue expected, but got Rvalue instead"
      else if (not $ checkTypeEq t exp_t) then
          return $ Just $ (show pos) ++  " - Mismatched argument type, expected: " ++ (show t) ++ " but got: " ++ (show exp_t)
          else
            checkFunArgs t_args t_exp

checkBlock :: Block -> CheckerT Env

checkBlock (Block _ l) = do
  env <- ask
  local (const (incLevel env)) (checkStmtList l)

  return env

checkStmtList :: [Stmt] -> CheckerT Env

checkStmtList [] = do
  env <- ask
  return env

checkStmtList (x:t) = do
  env <- checkStmt x
  local (const env) (checkStmtList t)

checkElif :: [Elif] -> CheckerT Env

checkElif [] = do
  env <- ask
  return env

checkElif ((Elif pos e b):l) = do
  assertType (Bool Nothing) e
  checkBlock b
  checkElif l

declareArg :: Arg -> CheckerT Env

declareArg (Arg pos t i) = do
  env <- declareType i t
  return env

declareArg (ArgRef pos t i) = do
  env <- declareType i t
  return env

declareArgs :: [Arg] -> CheckerT Env

declareArgs [] = do
  env <- ask
  return env

declareArgs (x:xs) = do
  env <- declareArg x
  local (const env) (declareArgs xs)

declareFun :: TopDef -> CheckerT Env

declareFun (FnDef pos ret_t i arg_l b) = do
  env <- declareType i (Fun pos ret_t arg_l)
  new_env <- local (const env) (declareArgs arg_l)
  local (const (incLevel $ setReturnType new_env ret_t)) $ checkBlock b
  return env

checkStmt :: Stmt -> CheckerT Env

checkStmt (Empty pos) = do
  env <- ask
  return env

checkStmt (BStmt pos b) = do
  env <- ask
  checkBlock b
  return env

checkStmt (Decl pos t []) = do
  env <- ask
  return env

checkStmt (Decl pos t (x:xs)) = do
  env <- ask
  case x of
    NoInit pos i -> do
      innerEnv <- declareType i t
      local (const innerEnv) (checkStmt (Decl pos t xs))
    Init pos i e -> do
      eType <- evalType e
      if (checkTypeEq t eType) then do
        innerEnv <- declareType i t
        local (const innerEnv) (checkStmt (Decl pos t xs))
      else
        throwError $ "Mismatched type of the assigned variable at line: " ++ (show pos)

checkStmt (InnerFun pos def) = declareFun def

checkStmt (Ass pos i e) = do
  t <- evalType e
  env <- ask
  case (Map.lookup i $ _envTypes env) of
    Just (v_t, pos2) ->
      if (checkTypeEq v_t t) then
        return env
      else do
        throwError $ "Mismatched type of the assigned vairable at line: " ++ (show pos)
    Nothing -> do
      throwError $ "Usage of an undeclared variable at position: " ++ (show pos)

checkStmt (TAss pos l e) = do
  env <- ask
  t <- evalType e
  case t of
    Tuple tpos l2 -> do
      when (not $ (length l) == (length l2))
        (throwError $ (show pos) ++ ": Not enough arguments to unpack a tuple.")

      typeList <- mapM (\(TItem pos i) -> evalType (EVar pos i)) l

      when (not $ checkTypeLists l2 typeList)
        (throwError $ (show pos) ++ ": Variable types did not match the unpacked types.")

      return env
    otherwise -> do
      throwError $ (show pos) ++ ": Only tuples can be unpacked."

checkStmt (Incr pos i) = do
  env <- ask
  case (Map.lookup i $ _envTypes env) of
    Just (Int _, _) -> return env
    Just (t, _) -> do throwError $ (show pos) ++ ": Expected Int type, but got: " ++ (printConstr t) ++ " instead."
    _ -> do throwError $ (show pos) ++ " variable undeclared"

checkStmt (Decr pos i) = do
  env <- ask
  case (Map.lookup i $ _envTypes env) of
    Just (Int _, _) -> return env
    Just (t, _) -> throwError $ (show pos) ++ ": Expected Int type, but got: " ++ (printConstr t) ++ " instead."
    _ -> throwError $ (show pos) ++ " variable undeclared"


checkStmt (Ret pos e) = do
  env <- ask
  case (_envReturnType env) of
    Just t -> do
      _ <- assertType t e
      return env
    Nothing -> return env

checkStmt (VRet pos) = do
  env <- ask
  case (_envReturnType env)  of
    Just (Void _) -> return env
    Just x -> throwError $ "Invalid return type at position: " ++ (show $ hasPosition x)
    Nothing -> return env

checkStmt (Cond pos e b) = do
  assertType (Bool Nothing) e
  checkBlock b

checkStmt (CondElif pos e b l b_else) =do
  assertType (Bool Nothing) e
  checkBlock b
  checkElif l
  checkBlock b_else

checkStmt (While pos e st) = do
  assertType (Bool Nothing) e
  checkStmt st

checkStmt (For pos var_t var_i var_expr var_check var_st st) = do
  assertType var_t var_expr
  env <- ask
  env2 <- declareType var_i var_t

  local (const env2) (assertType (Bool Nothing) var_check)
  local (const env2) (checkStmt var_st)
  local (const env2) (checkStmt st)

  return env

--TODO - zastanowic sie czy tutaj powinienem sprawdzac czy jestem w petli
checkStmt (Break pos) = do
  env <- ask
  return env

checkStmt (Continue pos) = do
  env <- ask
  return env

checkStmt (SExp pos e) = do
  env <- ask
  evalType e
  return env

checkStmt (Print pos e) = do
  env <- ask
  t <- mapM evalType e
  if (and $ fmap ifPrintable t) then
    return env
  else
    throwError $ "Expression at position: " ++ (show pos) ++ " is not printable!"

checkProgram :: Program -> CheckerT Env

checkProgram (Program pos []) = do
  env <- ask
  return env

checkProgram (Program pos (x:xs)) = do
  env <- declareFun x
  local (const env) $ checkProgram (Program pos xs)

ifPrintable :: Type -> Bool

ifPrintable (Tuple a l) = and $ fmap ifPrintable l

ifPrintable t = printConstr t `elem` ["Str", "Int", "Bool"]
