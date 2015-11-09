import System.IO

{-main = do
     handle <- openFile "testFile.txt" ReadMode
     contents <- hGetContents handle
     putStr contents
     hClose handle-}

main = withFile "testFile.txt" ReadMode
     (\handle -> do
              contents <- hGetContents handle
              putStr contents)
       
