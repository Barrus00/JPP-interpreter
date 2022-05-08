import AbsGrammar

data StaticException =
  MismatchedType Type Type |
  MissingExpression Type |
  NotEnoughArgs Type BNFC'Position |
  TooManyArgs Type |
  InvalidReturnType Type |
  NotATuple Type |
  NotApplicative Type |
  NotVariable

printConstr = heads . words. show

printBnfcPos t = show $ fromJust $ hasPosition t

instance Show StaticException where
  show MismatchedType t1 t2 =
    "Expected type: " ++ printConstr t1 ++ " got type: " ++ printConstr t2 ++ " on line " ++ show $ fromJust $ hasPosition t2
  show MissingExpression t =
    "Missing an expression on line: "  ++ printBnfcPos t
  show NotEnoughArgs t pos =
    "Not enough arguments for calling the function defined: " ++ printBnfcPos t
