{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where



main :: IO ()
main = do
    let !x = 1 + 1
    putStrLn "Hello, Haskell!"


