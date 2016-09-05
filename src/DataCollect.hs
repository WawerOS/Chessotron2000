
module DataCollect(
saveSequence,
readSequence,
getInput,
showGame
) where

import Chess
import Tree
import Control.Applicative()
import System.IO
import System.Exit
import Control.Monad

data Zipper a = Zip [a] a [a]

listToZip :: [a] -> Zipper a
listToZip x = Zip [] (last x) (reverse (init x))

forwardNSteps :: Int -> Zipper a -> Zipper a
forwardNSteps _ z@(Zip _ _ [])= z
forwardNSteps 0 z = z
forwardNSteps n (Zip xs x (l:ls)) = forwardNSteps (n-1) (Zip (xs ++ [x]) l ls)

backNSteps :: Int -> Zipper a -> Zipper a
backNSteps _ z@(Zip [] _ _)= z
backNSteps 0 z = z
backNSteps n (Zip xs x ls) = backNSteps (n-1) (Zip (init xs) (last xs) (x:ls))

flushOut :: IO ()
flushOut = hFlush stdout

saveSequence :: FilePath  -> [(Move,Board)] -> IO ()
saveSequence fp sq = writeFile fp (show sq)

readSequence :: FilePath -> IO [(Move,Board)]
readSequence fp = read <$> readFile fp

getInput :: IO String
getInput = putStr ">>" >> flushOut >> getLine

evalInput :: String -> Zipper (Move,Board) -> IO ()
evalInput ":q" _ = exitSuccess
evalInput "" z = showZip $ forwardNSteps 1 z
evalInput ":b" z = showZip $ backNSteps 1 z
evalInput ":f" z = showZip $ forwardNSteps 1 z
evalInput _ z = putStrLn "Not a command!!" >> getInput >>= (\(x:xs) -> evalInput xs z)

showZip :: Zipper (Move,Board) -> IO ()
showZip (Zip [] a []) = print a >> flushOut
showZip z@(Zip ls l sl) = do
  let num = 1 + length ls
  putStrLn ("Num := " ++ show num ++ " of " ++ show (num + length sl))
  flushOut
  putStrLn (showNice (fst l))
  flushOut
  print (snd l)
  str <- getInput
  evalInput str z


showGame ::[(Move,Board)] -> IO ()
showGame = showZip . listToZip
