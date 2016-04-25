
module Chess (

{-
 PieceType is used to represent the different
 of pieces on the board
-}
PieceType(),
-- Color is used to represent a pieces side
Color(White,Black),
--  Piece is used to represent a combined pieceType and Color
Piece(),
Board(),
Move(Move,NoMove),
newBoard,
easyMove,
strategyVal,
getAllMoves,
isValidCoord,
makeMove,
newLine,
opposite,
check,
checkMate,
getAllOurMoves
) where

 import Data.Array
 import Data.Char
 import Text.Printf

{-
Provides data structures and functions
to model a  chess boards

Tasks:
- Reorganize functions
-}

 -- Some useful definitions
 newLine :: String
 newLine = printf "\n"
 interrupt :: String
 interrupt = replicate 17 '-' ++ newLine

 -- Used to represent a chess pieces rank, includes the rank of a empty space
 data PieceType = EmptyType | King | Queen | Bishop | Knight | Rook | Pawn
   deriving Eq

 instance Show PieceType where
  show EmptyType = " "
  show King = "k"
  show Queen = "q"
  show Bishop = "b"
  show Knight = "n"
  show Rook  = "r"
  show Pawn = "p"

 -- Used to represent a pieces side, includes represention of the the side of a empty space
 data Color  = Black | White | EmptyColor
   deriving (Eq,Show,Read)

-- Allows us to have a handy reference for the "other side"
 opposite :: Color -> Color
 opposite EmptyColor = EmptyColor
 opposite Black = White
 opposite White = Black

-- Abstraction of an actual chess piece
 data Piece = Piece Color PieceType
   deriving Eq

 instance Show Piece where
  show (Piece Black p) = map toUpper (show p)
  show (Piece White p) = show p
  show (Piece EmptyColor _) = " "

{-
 The next two functions provide acess to the
 components of a Piece
-}

 getPieceType :: Piece ->  PieceType
 getPieceType (Piece _ p) = p

 getColor :: Piece -> Color
 getColor (Piece c _) = c

 -- A representation of a Chess Board indexed on (0,0) to (7,7)
 newtype Board = Board (Array (Integer,Integer) Piece)
  deriving Eq
 instance Show Board where
   show (Board a) = printBoard a 7

 -- Prints a board
 printBoard :: Array (Integer,Integer) Piece -> Integer -> String
 printBoard a 0 = "|" ++ concat [ show (a ! (0,y)) ++ "|" | y <- [0..7]] ++ newLine ++ interrupt
 printBoard a x = "|" ++ concat [ show (a ! (x,y)) ++ "|" | y <- [0..7]] ++ newLine ++ interrupt ++ printBoard a (x-1)

 -- A holder for two tuples makes things easier to interface with
 data Move = Move (Integer,Integer) (Integer,Integer) | NoMove
  deriving Eq

 instance Show Move where
   show (Move z z') = "Moving " ++ show z ++ " to " ++ show z'
   show NoMove = "Move? What Move?"

 -- A constant for a empty space on the Chess board
 emptyPiece :: Piece
 emptyPiece = Piece EmptyColor EmptyType

 -- A helper function to generate a new board
 pieceNum :: (Eq a,Num a) => a -> PieceType
 pieceNum y | y == 0 || y == 7 = Rook
 pieceNum y | y == 1 || y == 6 = Knight
 pieceNum y | y == 2 || y == 5 = Bishop
 pieceNum y | y == 3 = Queen
 pieceNum y | y == 4 = King
 pieceNum _ = EmptyType

 -- linePawns produce a line of pawns for a specific color for a new board
 linePawns :: Color -> [((Integer,Integer),Piece)]
 linePawns Black = map (\y -> ((6,y),Piece Black Pawn)) [0..7]
 linePawns White = map (\y -> ((1,y),Piece White Pawn)) [0..7]

 -- lineBack produces the back line of the board with correct associations
 lineBack :: Color -> [((Integer,Integer),Piece)]
 lineBack Black = map (\y -> ((7,y),Piece Black (pieceNum y))) [0..7]
 lineBack White = map (\y -> ((0,y),Piece White (pieceNum y))) [0..7]

 -- Produces a line of empty spaces
 emptyLine :: Integer -> [((Integer,Integer),Piece)]
 emptyLine x = map (\y -> ((x,y),emptyPiece)) [0..7]

 -- A fresh board
 newBoard :: Board
 newBoard = Board $ array ((0,0),(7,7)) $ concat ([linePawns,lineBack] <*> [Black,White]) ++ concatMap emptyLine [2..5]

 -- get a piece from the board
 getPiece :: Board -> (Integer,Integer) -> Piece
 getPiece (Board a) z = a ! z

 -- Moves a piece from z to z' replaceing anything there
 movePiece :: Board -> (Integer,Integer) -> (Integer,Integer) -> Board
 movePiece (Board a) z z' = Board ( a // [(z,emptyPiece),(z',a ! z)])

 -- implements Move interface
 makeMove :: Move -> Board -> Board
 makeMove (Move z z') b = movePiece b z z'
 makeMove NoMove b = b

 -- takes a tuple and  checks if it is a valid coordinate
 isValidCoord :: (Integer,Integer) -> Bool
 isValidCoord (x,y) = all (\a -> (a <= 7) && (a >= 0)) [x,y]

 -- given a board and position returns all possible move to there
 whatCanGoThere :: Board -> (Integer,Integer) -> [(Integer,Integer)]
 whatCanGoThere b z = filter (\z' -> checkMove b z' z) (filter isValidCoord (map (adder z) deltas))

 -- Tells you wheather a position can be attacked
 attackable :: Board -> (Integer,Integer) -> Bool
 attackable b z = any (\z' -> clrGetter z' == opposite clr) (whatCanGoThere b z)
   where
    clr = getColor (getPiece b z)
    clrGetter = getColor . getPiece b

 -- gives back all possible move for a particular board
 getAllMoves :: Board -> [(Move,Board)]
 getAllMoves b = concatMap (getBoards b) (filterBoard isPiece b)

 -- For a particular board and piece gives all possible moves and boards
 getBoards :: Board -> (Integer, Integer) -> [(Move, Board)]
 getBoards b z = map (\z' -> (Move z z',movePiece b z z')) (moves b z)

 -- Is a piece on our side
 isOurSide :: Color -> Piece -> Bool
 isOurSide clr (Piece clr' _) = clr == clr'

 getAllOurMoves :: Color -> Board -> [(Move,Board)]
 getAllOurMoves c b = filter ((\(Move z _) -> isOurSide c (getPiece b z)) . fst) $ getAllMoves b

 isPiece :: Board  -> (Integer,Integer) -> Bool
 isPiece b = (/= EmptyColor) . getColor . getPiece b

 deltas :: [(Integer,Integer)]
 deltas = concatMap mult $ [(0,dy)| dy <- [1..7]] ++ [(dx,0)| dx <- [1..7]] ++ [(d,d) | d <- [1..7]] ++ [(1,2),(2,1)]

 moves :: Board -> (Integer,Integer) -> [(Integer,Integer)]
 moves b z = filter (checkMove b z) (filter isValidCoord (map (adder z) deltas))

 adder :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
 adder (x',y') (x'',y'') = (x'+x'',y'+y'')

 mult :: (Integer,Integer) -> [(Integer,Integer)]
 mult (0,y') = [(0,y'),(0,-y')]
 mult (x',0) = [(-x',0),(x',0)]
 mult (x',y') = [(-x',y'),(x',-y'),(-x',-y'),(x',y')]

 checkMate :: Color -> Board -> Bool
 checkMate EmptyColor _ = False
 checkMate c b = check c b && all (check c) (map snd $ getAllMoves b)

 check :: Color -> Board -> Bool
 check EmptyColor _ = False
 check c b = any (attackable b) (filterBoard filterKings b)
   where
     ourKing = Piece c King
     filterKings b' z = getPiece b' z == ourKing


 filterBoard :: (Board -> (Integer,Integer) -> Bool) -> Board -> [(Integer,Integer)]
 filterBoard f b = helper (0,0) []
  where
    helper :: (Integer,Integer) -> [(Integer,Integer)] -> [(Integer,Integer)]
    helper (x,y) xs | (x == 7) && (y == 7) = if f b (x,y) then (x,y):xs else xs
    helper (x,y) xs | y == 7 = if f b (x,y) then helper (x+1,0) ((x,y):xs) else helper (x+1,0) xs
    helper (x,y) xs = if f b (x,y) then helper (x,y+1) ((x,y):xs) else helper (x,y+1) xs

 blckedAt :: Board -> (Integer,Integer) -> (Integer,Integer) -> Maybe (Color,(Integer,Integer))
 blckedAt b z z'
  | getPiece b z == emptyPiece = Nothing
  | getPieceType (getPiece b z) == Knight = if EmptyColor /= clr' then Just (clr',z') else Nothing
  | otherwise = getFirstOnPath b z z' >>= (\s -> Just (getColor (getPiece b s),s))
    where
      clr' = getColor (getPiece b z')

 getFirstOnPath :: Board -> (Integer,Integer) -> (Integer,Integer) -> Maybe (Integer,Integer)
 getFirstOnPath b z@(x,y) z'@(x',y') = helper (x+incX,y+incY)
    where
      incX = signum dX
      incY = signum dY
      dX = x' - x
      dY = y' - y
      helper p | not (isValidCoord p) = Nothing
      helper (coordX,coordY) | coordX == x' && coordY == y' = if getPiece b z' == emptyPiece then Nothing else Just z'
      helper (coordX,coordY) = if getPiece b (coordX, coordY) /= emptyPiece
                                 then Just (coordX,coordY) else helper (coordX+incX,coordY+incY)

 sameOrNot :: Color -> (Integer,Integer) -> Maybe (Color,(Integer,Integer)) -> Bool
 sameOrNot _ _ Nothing = False
 sameOrNot clr _ (Just (clr',_)) | clr == clr' = True
 sameOrNot clr z (Just (clr',z')) | clr /= clr' = z /= z'
 sameOrNot _ _ _ = True

 checkMove :: Board -> (Integer,Integer) -> (Integer,Integer) -> Bool
 checkMove b z@(x,y) z'@(x',y')
  | not (isValidCoord z && isValidCoord z') = False
  | getPiece b z == emptyPiece = False
  | clr == clr' = False
  | pieceType == Knight = ((dX == 1) && (dY == 2)) || ((dX == 2) && (dY == 1))
  | sameOrNot clr z' blcked = False
  | pieceType == King = elem 1 [dX,dY] && (oneDir || eq) && not (check clr (movePiece b z z'))
  | pieceType == Queen = oneDir || eq
  | pieceType == Rook = oneDir
  | pieceType == Bishop = eq
  | pieceType == Pawn = ((clr' == opposite clr) && eq && (clrDir*(x'- x) == 1)) ||
                           ((clrDir*(x' - x) == 1) && (dY == 0))
      where
         clrDir = pawnDir clr
         oneDir = (dX == 0) || (dY == 0)
         eq = dX == dY
         dX = abs $ x - x'
         dY = abs $ y - y'
         pieceType = getPieceType (getPiece b z)
         clr = getColor (getPiece b z)
         clr' = getColor (getPiece b z')
         blcked = blckedAt b z z'

 checkMove _ _ _ = False

 pawnDir :: Num a => Color -> a
 pawnDir EmptyColor = 0
 pawnDir White = 1
 pawnDir Black = -1


 checkedMove :: Board -> (Integer,Integer) -> (Integer,Integer) -> Maybe Board
 checkedMove b z z' | checkMove b z z' = Just $ movePiece b z z'
 checkedMove b z z' | not (checkMove b z z') = Nothing
 checkedMove _ _ _ = Nothing

 flip3 :: (a -> b -> c -> d) -> (b -> c -> a -> d)
 flip3 f = (\ b c a -> f a b c)

 easyMove :: (Integer,Integer) -> (Integer,Integer) -> Board -> Maybe Board
 easyMove = flip3 checkedMove

 pieceVal :: Num a => PieceType -> a
 pieceVal EmptyType = 0
 pieceVal King = 15
 pieceVal  Pawn = 1
 pieceVal  Knight = 3
 pieceVal  Bishop = 3
 pieceVal  Rook = 4
 pieceVal Queen = 9

 colorSign :: Num a => Color -> Color -> a
 colorSign c c' | c == c' = 1
 colorSign c c' | c /= c' = -1
 colorSign _ _ = 0

 posVal :: Board -> Color -> (Integer,Integer) -> Double
 posVal b c z = (pieceVal pceType) * (colorSign c clr) * sum (map valFunc (moves b z))
   where
     clr = getColor (getPiece b z)
     pceType = getPieceType (getPiece b z)
     valFunc =  sameAZero clr . getPiece b

 sameAZero :: Color -> Piece -> Double
 sameAZero clr (Piece clr' _) | clr' == clr = 0
 sameAZero _ p = pieceVal (getPieceType p)

 strategyVal :: Color -> Board -> Double
 strategyVal c b | checkMate c b = -1
 strategyVal c b | checkMate (opposite c) b = 1
 strategyVal c b | check c b = -0.9
 strategyVal c b | check (opposite c) b = 0.9
 strategyVal c b = tanh $ sum $ map (posVal b c) (filterBoard isPiece b)
