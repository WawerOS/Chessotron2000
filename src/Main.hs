import Tree
import Chess
import Data.Maybe
import System.IO
import Control.Monad
import Control.Applicative()

{-
  The UI for my chess program
  Tasks:
  - Allow user to play against another human or to play against the AI
-}

-- generates a full tree of moves
moveTree :: Color -> Tree (Move,Board) -> Integer -> Tree (Move,Board)
moveTree clr = applyNTimes id (getAllOurMoves clr . snd,getAllOurMoves clr' . snd)
  where
    clr' = opposite clr

-- First node of the game tree
firstNode :: Tree (Move,Board)
firstNode = Leaf (NoMove,newBoard)

-- Actual AI
chessAI :: Color ->  Tree (Move,Board) -> (Maybe Move,Maybe (Tree (Move,Board)) )
chessAI c b = (\n -> (n >>= (Just . fst <=< getValAt tr . (:[])), n >>= getSubTree tr . (:[]) )) ind
  where
    ind = maxmin $ applyAtEnds (const 0) ((:[]) . strategyVal c . snd) tr
    tr = moveTree c b 3

-- Converts a tuple of Maybe's to a Maybe tuple
tupleMaybe :: (Maybe a,Maybe b) -> Maybe (a,b)
tupleMaybe (Nothing,_) = Nothing
tupleMaybe (_,Nothing) = Nothing
tupleMaybe (Just a,Just b) = Just (a,b)

-- Final completed interface with the AI
chessAIMove c = tupleMaybe . chessAI c

-- IO interface for the AI
getAIMove :: Color -> Tree (Move,Board) -> IO (Move,Tree (Move,Board))
getAIMove c b = do
  let maybeTpl = chessAIMove c b
  let tpl = justOrError maybeTpl
  return tpl

-- So unsafe you wouldn't believe
justOrError :: Maybe a -> a
justOrError (Just a) = a
justOrError Nothing = error "The AI ran out of things to say."

-- Flush's the stdout so the user can actually see what's going on
flushOut :: IO ()
flushOut = hFlush stdout

-- Get's a single coordinate
getUserCoord :: IO (Integer,Integer)
getUserCoord = do
  putStr ">>"
  flushOut
  line <- getLine
  let maybeCoord = maybeRead line :: Maybe (Integer,Integer)
  let coord = maybeOrDefault maybeCoord (8,8)
  if isValidCoord coord
  then return coord
  else putStrLn "It's 0-7 not 1-8. It's confusing I know but what can ya do?" >> flushOut >> getUserCoord

-- Get's a User's move
getUserMove :: Tree (Move,Board) -> IO (Move,Tree (Move,Board))
getUserMove tr = do
  z <- getUserCoord
  putStrLn $ "Moving " ++ show z ++ " to ..."
  flushOut
  z' <- getUserCoord
  let b = maybeOrDefault (getValAt tr []) (NoMove,newBoard)
  let mv = Move z z'
  return (mv,Leaf (mv,makeMove mv (snd b)))

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
playAGame :: (Tree (Move,Board) -> IO (Move,Tree (Move,Board))) -> (Tree (Move,Board) -> IO (Move,Tree (Move,Board))) -> Tree (Move,Board) -> IO ()
playAGame whitePlayer blackPlayer b = do

  mvRec <- whitePlayer b
  let b' = getValAt (snd mvRec) [] >>= Just . snd
  putStrLn $ showMaybe b' ++ newLine ++ "White: " ++ show (fst mvRec)

  mvRec' <-   blackPlayer (snd mvRec)
  let b'' =  getValAt (snd mvRec') [] >>= Just . snd
  putStrLn $ showMaybe b'' ++ newLine ++ "Black: " ++ show (fst mvRec')

  winner <- winnerCheck (maybeOrDefault b'' newBoard)
  if winner
  then winnerReward (maybeOrDefault b'' newBoard)
  else playAGame whitePlayer blackPlayer (snd mvRec')

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
colorMatcher :: Color -> (Tree (Move,Board) -> IO (Move,Tree (Move,Board))) -> (Tree (Move,Board) -> IO (Move,Tree (Move,Board))) -> IO ()
colorMatcher c player1 player2 | c == White = playAGame player1 player2 firstNode
colorMatcher c player1 player2 | c == Black = playAGame player2 player1 firstNode
colorMatcher _ _ _ = putStrLn "What have you done!?!" >> flushOut

-- The main function
main :: IO ()
main = do
  clr <- getColor
  let aiColor = opposite clr
  colorMatcher clr getUserMove (getAIMove aiColor)
