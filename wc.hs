main :: IO ()
main = do (nlines, nwords, nbytes) <- return . tac =<< getContents
          putStrLn (rjust 6 (show nlines) ++ " " ++
                    rjust 6 (show nwords) ++ " " ++
                    rjust 6 (show nbytes) ++ "\t-")

rjust :: Int -> String -> String
rjust width s = replicate (width - length s) ' ' ++ s

-- FIXME: lines wrongly removes LF.
tac :: String -> (Int, Int, Int)
tac = foldl add3 (0,0,0) . map count . lines

add3 :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
add3 (i,j,k) (i',j',k') = (i + i', j + j', k + k')

count :: String -> (Int, Int, Int)
count line = (1, length (words line), length line)
