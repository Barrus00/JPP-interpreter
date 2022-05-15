import ParGrammar (pProgram, myLexer)
import TypeChecker (runChecker, checkProgram)
import Interpeter (runInterpreter)
import System.IO

main :: IO ()
main = do
    s <- readFile "../examples/example_tuples.tipa"
    case (pProgram $ myLexer s) of
      Left err -> hPutStrLn stderr err
      Right program ->
        case (runChecker program) of
          Left err -> hPutStrLn stderr $ "type error failure: " ++ err
          Right _ -> hPutStrLn stderr "OK!"


main3 :: IO ()
main3 = do
    s <- readFile "../examples/example_recurrence.tipa"
    case (pProgram $ myLexer s) of
      Left err -> hPutStrLn stderr err
      Right program ->
        case (runChecker program) of
          Left err -> hPutStrLn stderr $ "type error failure: " ++ err
          Right _ -> do
            (err, st) <- runInterpreter program
            case (err) of
              Left err -> hPutStrLn stderr $ "Runtime error: " ++ err
              Right _ -> hPutStrLn stderr $ "OK!"

main2 :: IO ()

main2 = do
    s <- readFile "../examples/example_lambda.tipa"
    putStr $ show $ pProgram $ myLexer s
