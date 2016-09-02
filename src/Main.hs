import           Chess
import           Control.Applicative ()
import           Control.Monad
import           Data.Maybe
import           System.IO
import           Tree
import           System.Exit

{-
  The UI for my chess program
  Tasks:
  - Allow user to play against another human or to play against the AI
-}

-- generates a full tree of moves
moveTree :: Color -> (Move,Board) -> Integer -> Tree (Move,Board)
moveTree clr = applyNTimes id (getAllOurMoves clr . snd,getAllOurMoves clr' . snd) . Leaf
  where
    clr' = opposite clr

-- First node of the game tree
firstNode :: (Move,Board)
firstNode = (NoMove,newBoard)

-- How far the rabbit hole goes
moveTreeDepth :: Integer
moveTreeDepth = 3

-- Actual AI
chessAI :: Color ->  [(Move,Board)] -> Maybe (Move,Board)
chessAI c (b:_) = b'
  where
    a = moveTree c b moveTreeDepth
    valTr = fmap (strategyVal c . snd) a
    ind = chooser (alphaBeta (-1/0) (1/0) True) valTr
    b' = ind >>= (\v -> getValAt a [v])

-- Converts a tuple of Maybe's to a Maybe tuple
tupleMaybe :: (Maybe a,Maybe b) -> Maybe (a,b)
tupleMaybe (Nothing,_) = Nothing
tupleMaybe (_,Nothing) = Nothing
tupleMaybe (Just a,Just b) = Just (a,b)

-- IO interface for the AI
getAIMove :: Color -> [(Move,Board)] -> IO (Move,Board)
getAIMove c b = do
  let maybeTpl = chessAI c b
  let tpl = maybeOrDefault maybeTpl (NoMove,newBoard)
  return tpl

-- Flush's the stdout so the user can actually see what's going on
flushOut :: IO ()
flushOut = hFlush stdout

combine :: (Bool,Bool) -> Bool
combine (a,a') = a && a'

beforeChoice = do
  putStr ">>"
  flushOut
  getLine

afterChoice m mv= do
  print mv
  flushOut
  return (mv,makeMove mv (snd m))


-- Get's a User's move
getUserMove :: [(Move,Board)] -> IO (Move,Board)
getUserMove [m] = do
  input <- beforeChoice
  when (input ==  ":q") exitSuccess
  if input == ":b"
    then putStrLn "There's nothing there!!!!" >> getUserMove [m]
    else do
      let mv = maybeOrDefault (maybeRead input) NoMove :: Move
      if mv == NoMove then putStrLn (newLine ++"Please enunciate") >> getUserMove [m]
        else afterChoice m mv

getUserMove [m,_] = getUserMove [m]

getUserMove ls@(m:_:ms) = do
  input <- beforeChoice
  when (input ==  ":q") exitSuccess
  if input == ":b"
    then print (snd $ head ms) >> getUserMove ms
    else do
      let mv = maybeOrDefault (maybeRead input) NoMove :: Move
      if mv == NoMove then putStrLn "Please enunciate" >> getUserMove ls
        else afterChoice m mv

-- Checks wheather some one has won yet
winnerCheck :: Board -> IO Bool
winnerCheck b = return $ any (`checkMate` b) [Black,White]

-- End the game
winnerReward :: Board -> IO ()
winnerReward b = do
  let clr = head $ filter (`checkMate` b) [Black,White]
  putStrLn $ "Congratulations " ++ show (opposite clr) ++ " you have won the Esteemed game of chess!!"

-- Shows nothing if Nothing and something if Just
showMaybe :: Show a => Maybe a -> String
showMaybe Nothing = ""
showMaybe (Just a) = show a

-- The function used for playing the game
playAGame :: ([(Move,Board)] -> IO (Move,Board)) -> ([(Move,Board)] -> IO (Move,Board)) -> [(Move,Board)] -> IO ()
playAGame whitePlayer blackPlayer m = do

  mvRec <- whitePlayer m
  let b' =  snd mvRec
  putStrLn $ show b' ++ newLine ++ "White: " ++ show (fst mvRec)

  mvRec' <-   blackPlayer (mvRec:m)
  let b'' =  snd mvRec'
  putStrLn $ show b'' ++ newLine ++ "Black: " ++ show (fst mvRec')

  winner <- winnerCheck b''
  if winner
  then winnerReward b''
  else playAGame whitePlayer blackPlayer (mvRec':mvRec:m)

-- Returns Nothing if  there's nothing and a Just if there is
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- Guess what this does
maybeOrDefault :: Maybe a -> a -> a
maybeOrDefault (Just a) _ = a
maybeOrDefault Nothing a = a

-- Tells us if there's something in the variable
isThere :: Maybe a -> Bool
isThere (Just _) = True
isThere Nothing = False

-- Get's user's Color
getColor :: IO Color
getColor = do
  putStrLn "So your'e here for a  game eh!"
  flushOut
  putStrLn "So what Color, Black or White?"
  flushOut
  msg <- getLine
  let valList = maybeRead msg :: Maybe Color
  if isThere valList
  then return $ maybeOrDefault valList White
  else putStrLn "Use capitals. I'm a stickler for grammar!" >> flushOut >> getColor

-- Matches colors to  proper inputs
colorMatcher :: Color -> ([(Move,Board)] -> IO (Move,Board)) -> ([(Move,Board)] -> IO (Move,Board)) -> IO ()
colorMatcher c player1 player2 | c == White = print (snd firstNode) >> playAGame player1 player2 [firstNode]
colorMatcher c player1 player2 | c == Black = playAGame player2 player1 [firstNode]
colorMatcher _ _ _ = putStrLn "What have you done!?!" >> exitFailure

-- The main function
main :: IO ()
main = do
  clr <- getColor
  let aiColor = opposite clr
  colorMatcher clr getUserMove (getAIMove aiColor)
