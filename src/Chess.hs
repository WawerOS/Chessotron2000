
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
Pos,
Move(Move,NoMove),
numToPos,
mapMove,
newBoard,
easyMove,
strategyVal,
showNice,
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
 import Control.Arrow

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

 instance Read PieceType where
  readsPrec _ ('k':xs) = [(King,xs)]
  readsPrec _ ('q':xs) = [(Queen,xs)]
  readsPrec _ ('b':xs) = [(Bishop,xs)]
  readsPrec _ ('n':xs) = [(Knight,xs)]
  readsPrec _ ('r':xs) = [(Rook,xs)]
  readsPrec _ ('p':xs) = [(Pawn,xs)]

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

 instance Read Piece where

   readsPrec _ (chr:str) | isUpper chr = [(Piece Black pType,str)]
    where
      pType = read [toLower chr] :: PieceType
   readsPrec _ (chr:str) | isLower chr = [(Piece White (read [chr]),str)]
   readsPrec _ (' ':str) = [(emptyPiece,str)]
   readsPrec _ _ = []

{-
 The next two functions provide acess to the
 components of a Piece
-}

 getPieceType :: Piece ->  PieceType
 getPieceType (Piece _ p) = p

 getColor :: Piece -> Color
 getColor (Piece c _) = c

 type Pos = (Integer,Integer)

 -- A representation of a Chess Board indexed on (0,0) to (7,7)
 newtype Board = Board (Array (Integer,Integer) Piece)
  deriving Eq
 instance Show Board where
   show (Board a) = printBoard a 7

 instance Read Board where
   readsPrec _ string | length strBoard /= 64 = []
    where
      strBoard = filter (\x -> x /= '-' && x /= '\n'
                            && not (isNumber x) && x /= '|') $ take 296 string
   readsPrec _ string = [(board,xs)]
    where
      xs = drop 314 string
      strBoard = filter (\x -> x /= '-' && x /= '\n'
                            && not (isNumber x) && x /= '|') $ take 296 string
      assign _ [] = []
      assign j (p:str) = (numToPos j,read [p] :: Piece) : assign (j + 1) str
      board = Board $ array ((0,0),(7,7)) (assign 0 strBoard)

 numToPos :: Int -> Pos
 numToPos j  = (fromIntegral $ i `div` 8,fromIntegral $ j `mod` 8)
  where
    i = 63 - j

 -- Prints a board
 printBoard :: Array (Integer,Integer) Piece -> Integer -> String
 printBoard a (-1) = " " ++ concat [ show y ++ " " | y <- [0..7]] ++  newLine
 printBoard a x = "|" ++ concat [ show (a ! (x,y)) ++ "|" | y <- [0..7]] ++ show x ++ newLine ++ interrupt ++ printBoard a (x-1)

 -- A holder for two tuples makes things easier to interface with
 data Move = Move Pos Pos | EnPassant Pos Pos | Castle Pos Pos | NoMove
  deriving (Read,Eq,Show)

 showNice :: Move -> String
 showNice (Move z z') = "Moving " ++ show z ++ " to " ++ show z'
 showNice (Castle z z') = "Castling " ++ show z ++ " to " ++ show z'
 showNice (EnPassant z z') = "Funky French move " ++ show z ++ " to " ++ show z'
 showNice NoMove = "Move? What Move?"

 mapMove :: (Pos -> a) -> Move -> (a,a)
 mapMove f (Move z z') = (f z,f z')
 mapMove f (EnPassant z z') = (f z,f z')
 mapMove f (Castle z z') = (f z,f z')
 mapMove f NoMove = (f (0,0),f (0,0))

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
 linePawns :: Color -> [(Pos,Piece)]
 linePawns Black = map (\y -> ((6,y),Piece Black Pawn)) [0..7]
 linePawns White = map (\y -> ((1,y),Piece White Pawn)) [0..7]

 -- lineBack produces the back line of the board with correct associations
 lineBack :: Color -> [(Pos,Piece)]
 lineBack Black = map (\y -> ((7,y),Piece Black (pieceNum y))) [0..7]
 lineBack White = map (\y -> ((0,y),Piece White (pieceNum y))) [0..7]

 -- Produces a line of empty spaces
 emptyLine :: Integer -> [(Pos,Piece)]
 emptyLine x = map (\y -> ((x,y),emptyPiece)) [0..7]

 -- A fresh board
 newBoard :: Board
 newBoard = Board $ array ((0,0),(7,7)) $ concat ([linePawns,lineBack] <*> [Black,White]) ++ concatMap emptyLine [2..5]

 -- get a piece from the board
 getPiece :: Board -> Pos -> Piece
 getPiece (Board a) z = a ! z

 -- Moves a piece from z to z' replaceing anything there
 movePiece :: Board -> Pos -> Pos -> Board
 movePiece (Board a) z z' = Board ( a // [(z,emptyPiece),(z',a ! z)])

 -- implements Move interface
 makeMove :: Move -> Board -> Board
 makeMove (Move z z') b = movePiece b z z'
 makeMove (Castle z z'@(x,y)) b@(Board a) = Board ( a // [(z',emptyPiece),(z,emptyPiece),(place,p),(place',p')])
   where
     p = getPiece b z
     p' = getPiece b z'
     place = if y == 0 then (x,2) else (x,6)
     place' = if y == 0 then (x,3) else (x,5)
 makeMove (EnPassant z z') b = movePiece (movePiece b z z') z' (adder z' (dir,0))
  where
    clr = getColor $ getPiece b z
    dir = pawnDir clr
 makeMove NoMove b = b

 -- takes a tuple and  checks if it is a valid coordinate
 isValidCoord :: Pos -> Bool
 isValidCoord (x,y) = all (\a -> (a <= 7) && (a >= 0)) [x,y]

 -- given a board and position returns all possible move to there
 whatCanGoThere :: Board -> Pos -> [Pos]
 whatCanGoThere b z = filter (\z' -> checkMove b z' z) (filter isValidCoord (map (adder z) deltas))

 -- Tells you wheather a position can be attacked
 attackable :: Board -> Pos -> Bool
 attackable b z = any (\z' -> clrGetter z' == opposite clr) (whatCanGoThere b z)
   where
    clr = getColor (getPiece b z)
    clrGetter = getColor . getPiece b

 -- gives back all possible move for a particular board
 getAllMoves :: Board -> [(Move,Board)]
 getAllMoves b = concatMap (getBoards b) (filterBoard isPiece b)

 -- For a particular board and piece gives all possible moves and boards
 getBoards :: Board -> (Integer, Integer) -> [(Move, Board)]
 getBoards b z = map (Move z &&& movePiece b z) (moves b z)

 -- Is a piece on our side
 isOurSide :: Color -> Piece -> Bool
 isOurSide clr (Piece clr' _) = clr == clr'

 -- Procures all possible moves for a single side
 getAllOurMoves :: Color -> Board -> [(Move,Board)]
 getAllOurMoves c b = filter ((\(Move z _) -> isOurSide c (getPiece b z)) . fst) $ getAllMoves b

 -- Is it a piece?
 isPiece :: Board  -> Pos -> Bool
 isPiece b = (/= EmptyColor) . getColor . getPiece b

 -- All possible changes in position for any piece
 deltas :: [Pos]
 deltas = concatMap mult $ [(0,dy)| dy <- [1..7]] ++ [(dx,0)| dx <- [1..7]] ++ [(d,d) | d <- [1..7]] ++ [(1,2),(2,1)]

 -- Properly applies the concept of negatives to vectors
 mult :: Pos -> [Pos]
 mult (0,y') = [(0,y'),(0,-y')]
 mult (x',0) = [(-x',0),(x',0)]
 mult (x',y') = [(-x',y'),(x',-y'),(-x',-y'),(x',y')]

 -- All moves for a particular piece
 moves :: Board -> Pos -> [Pos]
 moves b z = filter (checkMove b z) (filter isValidCoord (map (adder z) deltas))

 -- Adding, but for vectors
 adder :: Pos -> Pos -> Pos
 adder (x',y') (x'',y'') = (x'+x'',y'+y'')

 -- Check if a side is in checkmate
 checkMate :: Color -> Board -> Bool
 checkMate EmptyColor _ = False
 checkMate c b = (check c b && all (check c) (map snd $ getAllMoves b)) ||
                    null (filterBoard (\b z -> (getPiece b z == Piece c King)) b)

 -- Checks if a side is in check
 check :: Color -> Board -> Bool
 check EmptyColor _ = False
 check c b = any (attackable b) (filterBoard filterKings b)
   where
     ourKing = Piece c King
     filterKings b' z = getPiece b' z == ourKing

 -- Finds all pieces on a board that have a certain, ... something
 filterBoard :: (Board -> Pos -> Bool) -> Board -> [Pos]
 filterBoard f b = helper (0,0) []
  where
    helper :: Pos -> [Pos] -> [Pos]
    helper (x,y) xs | (x == 7) && (y == 7) = if f b (x,y) then (x,y):xs else xs
    helper (x,y) xs | y == 7 = if f b (x,y) then helper (x+1,0) ((x,y):xs) else helper (x+1,0) xs
    helper (x,y) xs = if f b (x,y) then helper (x,y+1) ((x,y):xs) else helper (x,y+1) xs

 -- Tells you if a move is blocked by a piece
 blckedAt :: Board -> Pos -> Pos -> Maybe (Color,Pos)
 blckedAt b z z'
  | getPiece b z == emptyPiece = Nothing
  | getPieceType (getPiece b z) == Knight = if EmptyColor /= clr' then Just (clr',z') else Nothing
  | otherwise = getFirstOnPath b z z' >>= (\s -> Just (getColor (getPiece b s),s))
    where
      clr' = getColor (getPiece b z')

 -- Gets the first piece on the path made by two places
 getFirstOnPath :: Board -> Pos -> Pos -> Maybe Pos
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

 -- Checks if the path is blocked accounting for taking pieces
 sameOrNot :: Color -> Pos -> Maybe (Color,Pos) -> Bool
 sameOrNot _ _ Nothing = False
 sameOrNot clr _ (Just (clr',_)) | clr == clr' = True
 sameOrNot clr z (Just (clr',z')) | clr /= clr' = z /= z'
 sameOrNot _ _ _ = True

 -- Checks if a move is allowed, en'pasant and castling not implemented
 checkMove :: Board -> Pos -> Pos -> Bool
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
                           ((clrDir*(x' - x) == 1) && (dY == 0) && (clr' == EmptyColor))
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

 -- Tells you which direction a pawn moves based on color
 pawnDir :: Num a => Color -> a
 pawnDir EmptyColor = 0
 pawnDir White = 1
 pawnDir Black = -1

 -- Allows for easier chaining of moves with Maybe
 checkedMove :: Board -> Pos -> Pos -> Maybe Board
 checkedMove b z z' | checkMove b z z' = Just $ movePiece b z z'
 checkedMove b z z' | not (checkMove b z z') = Nothing
 checkedMove _ _ _ = Nothing

 -- Convience function
 flip3 :: (a -> b -> c -> d) -> (b -> c -> a -> d)
 flip3 f b c a = f a b c

 -- Allows easier chaining of moves
 easyMove :: Pos -> Pos -> Board -> Maybe Board
 easyMove = flip3 checkedMove

 -- Assigns a strategic value to each piece
 pieceVal :: Num a => PieceType -> a
 pieceVal EmptyType = 0
 pieceVal King = 10
 pieceVal  Pawn = 1
 pieceVal  Knight = 3
 pieceVal  Bishop = 7
 pieceVal  Rook = 4
 pieceVal Queen = 8

 oneSidePieceScore :: Double
 oneSidePieceScore = sum (map (pieceVal . getPieceType . getPiece newBoard) $ filterBoard isPiece newBoard)/2

 sumPieceScore :: Color -> Board -> Double
 sumPieceScore clr brd = sum $ map (pieceVal . getPieceType . getPiece brd) $ filterBoard (\b -> isOurSide clr . getPiece b) brd

 -- Convience function for comparing colors and getting coeffiencts
 colorSign :: Num a => Color -> Color -> a
 colorSign c c' | c == c' = 1
 colorSign c c' | c /= c' = -1
 colorSign _ _ = 0

 -- Assingns a value to a piece based on Color, places to move, and pieces at those places
 posVal :: Board -> Color -> Pos -> Double
 posVal b c z = colorSign c clr * sum (map valFunc (moves b z))
   where
     clr = getColor (getPiece b z)
     pceType = getPieceType (getPiece b z)
     valFunc =  sameAZero clr . getPiece b

 -- Its of no value if you can move into your own places
 sameAZero :: Color -> Piece -> Double
 sameAZero clr piece | isOurSide clr piece = 0
 sameAZero _ p = pieceVal (getPieceType p)

 -- Computes the strategic value of a board
 strategyVal :: Color -> Board -> Double
 strategyVal c b | checkMate c b = -1.5
 strategyVal c b | checkMate (opposite c) b = 1.5
 strategyVal c b | check c b = -1.2
 strategyVal c b = tanh ( sumPieceScore c b - sumPieceScore c' b)
  where
    c' = opposite c
