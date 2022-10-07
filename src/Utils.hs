module Utils (printConstr, showPos, showIdent, showType, showArg) where

import AbsGrammar 
import Data.List (intercalate)
import Data.Maybe

printConstr :: (Show a) => a -> String
printConstr = head . words . show

showPos :: BNFC'Position -> String

showPos (Nothing) = ""
showPos (Just (line, col)) = "(line " ++ (show line) ++ ":column " ++ (show col) ++ ")"

showIdent :: Ident -> String

showIdent (Ident x) = show x

showType :: Type -> String
showType (Tuple _ t_l) = "Tuple[" ++ (intercalate ", " $ fmap showType t_l) ++ "]"
showType (Fun _ t a_l) = "Fun(" ++ (intercalate ", " $ fmap showArg a_l) ++ ") => (" ++ showType t ++ ")"
showType (Lambda _ a_l t) = "Lambda(" ++ (intercalate ", " $ fmap showArg a_l) ++ ") => (" ++ showType t ++ ")"
showType t = printConstr t

showArg :: Arg -> String
showArg (Arg _ t i) = (showType t)
showArg (ArgRef _ t i) = "Ref" ++ (showType t)
