import Par (pProgram, myLexer)

main :: IO ()
main = do
    s <- readFile "example1.txt"
    putStr $ show $ pProgram $ myLexer s
