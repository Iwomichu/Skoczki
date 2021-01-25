data Color = Black | White deriving (Show, Eq)
data Position = Position Int Int deriving (Show, Eq)
type Finished = Bool
data Pawn = Pawn Position Finished Color deriving (Show, Eq)
data Move = Move Position Position deriving (Show, Eq)
data Direction = LeftDirection | RightDirection | UpDirection | DownDirection deriving (Show, Eq)
type BlackPawn = Pawn
type WhitePawn = Pawn
type Winner = Color
data BoardState = BoardState [WhitePawn] [BlackPawn]
data HumanMoveEvaluationResult = HumanMoveEvaluationResult Move | InvalidMove
data GameType = Multiplayer | Singleplayer

prettyPrintPawn :: Pawn -> [Char]
prettyPrintPawn (Pawn _ _ c) = case c of
    Black -> "| X "
    White -> "| O "

getPosition :: Pawn -> Position
getPosition (Pawn p _ _) = p

getX :: Position -> Int
getX (Position x y) = x

getY :: Position -> Int
getY (Position x y) = y

getAdjacentSquares :: Position -> [Position]
getAdjacentSquares (Position x y)  = [
        Position (x - 1) y, Position (x + 1) y, 
        Position x (y + 1), Position x (y - 1)
        ]

-- from HaskellWiki (http://www.haskell.org/haskellwiki/Introduction#Quicksort_in_Haskell), modified to use key function
quicksortPawn :: (Pawn -> Int ) -> [Pawn] -> [Pawn]
quicksortPawn _ []     = []
quicksortPawn key (p:xs) = (quicksortPawn key lesser) ++ [p] ++ (quicksortPawn key greater)
    where
        lesser  = filter (\pawn -> key p > key pawn) xs
        greater = filter (\pawn -> key p <= key pawn) xs

isMoveLegal :: Move -> Bool
isMoveLegal (Move _ (Position x y))
    | x < 0 = False
    | x >= 8 = False
    | y < 0 = False
    | y >= 8 = False
    | otherwise = True

filterOutIllegalMoves :: [Move] -> [Move]
filterOutIllegalMoves = filter isMoveLegal

sortAccordingToDirection :: Direction -> [Pawn] -> [Pawn]
sortAccordingToDirection direction
    | direction == RightDirection = quicksortPawn (\(Pawn (Position x y) _ _) -> x)
    | direction == LeftDirection = reverse . quicksortPawn (\(Pawn (Position x y) _ _) -> x)
    | direction == DownDirection = quicksortPawn (\(Pawn (Position x y) _ _) -> y)
    | direction == UpDirection = reverse . quicksortPawn (\(Pawn (Position x y) _ _) -> y)

getAdjacencyDirection :: Position -> Pawn -> Direction
getAdjacencyDirection (Position x1 y1) (Pawn (Position x2 y2) _ _)
    | x1 > x2 = LeftDirection
    | x1 < x2 = RightDirection
    | y1 > y2 = UpDirection
    | y1 < y2 = DownDirection

getPawnsOnAdjacentSquares :: Position -> [Pawn] -> [(Pawn, Direction)]
getPawnsOnAdjacentSquares p = map (\x -> (x, getAdjacencyDirection p x)) . filter (\(Pawn pos _ _) -> pos `elem` getAdjacentSquares p)

getPawnsOnStraightLine :: Position -> Pawn -> [Pawn] -> [Pawn]
getPawnsOnStraightLine (Position x1 y1) (Pawn (Position x2 y2) _ _) ps 
    | x1 < x2 = filter (\(Pawn (Position x y) _ _)-> x > x1) (filter (\(Pawn (Position x y) _ _)-> y == y2) ps)
    | x1 > x2 = filter (\(Pawn (Position x y) _ _)-> x < x1) (filter (\(Pawn (Position x y) _ _)-> y == y2) ps)
    | y1 < y2 = filter (\(Pawn (Position x y) _ _)-> y > y1) (filter (\(Pawn (Position x y) _ _)-> x == x2) ps)
    | y1 > y2 = filter (\(Pawn (Position x y) _ _)-> y < y1) (filter (\(Pawn (Position x y) _ _)-> x == x2) ps)
    | otherwise = []

getFirstEmptySquare :: Position -> [Pawn] -> Direction -> Position
getFirstEmptySquare (Position x1 y1) [] direction = case direction of
        LeftDirection -> Position (x1-1) y1
        RightDirection -> Position (x1+1) y1
        UpDirection -> Position x1 (y1-1)
        DownDirection -> Position x1 (y1+1)
getFirstEmptySquare (Position x1 y1) ps direction
    | direction == LeftDirection && x1 - x2 == 1 = getFirstEmptySquare (Position x2 y2) (tail sorted) direction
    | direction == RightDirection && x2 - x1 == 1 = getFirstEmptySquare (Position x2 y2) (tail sorted) direction
    | direction == UpDirection && y1 - y2 == 1 = getFirstEmptySquare (Position x2 y2) (tail sorted) direction
    | direction == DownDirection && y2 - y1 == 1 = getFirstEmptySquare (Position x2 y2) (tail sorted) direction
    | otherwise = case direction of
        LeftDirection -> Position (x1-1) y1
        RightDirection -> Position (x1+1) y1
        UpDirection -> Position x1 (y1-1)
        DownDirection -> Position x1 (y1+1)
    where
        sorted = sortAccordingToDirection direction ps
        (Pawn (Position x2 y2) _ _) = head sorted

getNextPosition :: Position -> (Pawn, Direction) -> [Pawn] -> Position
getNextPosition start (pawn, direction) ps = getFirstEmptySquare start (getPawnsOnStraightLine start pawn ps) direction

getJumpPositions :: Position -> [Position] -> [Pawn] -> [Position]
getJumpPositions currentPosition pastPositions pawns = currentPosition: (concatMap (\p -> getJumpPositions p newPastPositions pawns) $  filter (`notElem` newPastPositions) (map (\p -> getNextPosition currentPosition p pawns) adjacentPawns))
    where
        adjacentPawns = getPawnsOnAdjacentSquares currentPosition pawns
        newPastPositions = currentPosition:pastPositions

getPossiblePawnMoves :: Pawn -> [Pawn] -> [Move]
getPossiblePawnMoves (Pawn currentPosition f c) pawns = filterOutIllegalMoves $ map (Move currentPosition) (simpleNewPositions ++ jumpingNewPositions)
    where
        simpleNewPositions = filter (\square -> square `notElem` map (\(Pawn pos _ _) -> pos) pawns) $ getAdjacentSquares currentPosition
        jumpingNewPositions = tail $ getJumpPositions currentPosition [] (filter (/= Pawn currentPosition f c) pawns)

getAllPossibleColorMoves :: Color -> [Pawn] -> [Move]
getAllPossibleColorMoves color1 pawns = concatMap (`getPossiblePawnMoves` pawns) (filter (\(Pawn _ _ color2) -> color1 == color2) pawns)

getMoveGreedyValue :: Color -> Move -> Int
getMoveGreedyValue color (Move (Position x1 y1) (Position x2 y2))
    | color == Black = y2 - y1
    | color == White = y1 - y2

generateRowString :: Int -> [Pawn] -> String
generateRowString 0 [] = "|" ++ concat (replicate 8 "   |")
generateRowString 8 _ = "|"
generateRowString column [] = concat  (replicate (8 - column) "|   ") ++ "|"
generateRowString currentColumn pawns = if x == currentColumn then prettyPrintPawn (Pawn (Position x y) f c) ++ generateRowString (currentColumn+1) (tail sorted) else "|   " ++ generateRowString (currentColumn+1) sorted
    where
        sorted = quicksortPawn (\(Pawn (Position x _) _ _) -> x) pawns
        Pawn (Position x y) f c = head sorted

generateBoardRepr :: BoardState -> String
generateBoardRepr (BoardState white black) = unlines $ [generateRowString 0 $filter (\(Pawn (Position _ y) _ _) -> y == i) pawns | i <- [0..7]] ++ [""]
    where
        pawns = white ++ black

isFinal:: Pawn -> Bool
isFinal (Pawn (Position _ y) _ c) = case c of
    White -> y <= 1
    Black -> y >= 6

updatePawn:: Pawn -> Move -> Pawn
updatePawn (Pawn p f c) (Move _ newPosition) = Pawn newPosition (isFinal (Pawn newPosition f c)) c

performMove:: Move -> [Pawn] -> [Pawn]
performMove (Move oldPos newPos) ps = rest ++ [updatePawn element (Move oldPos newPos)]
    where
        element = head $ filter (\(Pawn pos _ _) -> pos == oldPos) ps
        rest = filter (\(Pawn pos _ _) -> pos /= oldPos) ps

allFinished :: [Pawn] -> Bool
allFinished = all isFinal

digitToInt :: Char -> Maybe Int
digitToInt c = lookup c (zip ['0'..'9'] [0..9])

opponentMove :: Color -> BoardState -> Move
opponentMove color (BoardState whitePawns blackPawns) = head $ filter (\move -> getMoveGreedyValue color move == max_value) possible_moves
    where
        possible_moves = getAllPossibleColorMoves color (whitePawns++blackPawns)
        max_value = maximum $ map (getMoveGreedyValue color) possible_moves

parseHumanPosition :: String -> [Position]
parseHumanPosition [] = []
parseHumanPosition (a:b:r) = Position x (y-1) : parseHumanPosition r
    where
        x = case a of
            'A' -> 0
            'B' -> 1
            'C' -> 2
            'D' -> 3
            'E' -> 4
            'F' -> 5
            'G' -> 6
            'H' -> 7
        y = case digitToInt b of
            Just value -> value
            Nothing -> 0

parseHumanInput :: String -> [Position]
parseHumanInput input = parseHumanPosition $ filter (/= '-') input

parseHumanColor :: [Char] -> Color
parseHumanColor inp = case head inp of
    'X' -> Black
    'O' -> White

moveToReadable :: Move -> String
moveToReadable (Move a b) = positionToReadable a ++ "-" ++ positionToReadable b

positionToReadable :: Position -> String
positionToReadable (Position x y) = case x of
    0 -> "A" ++ show (y+1)
    1 -> "B" ++ show (y+1)
    2 -> "C" ++ show (y+1)
    3 -> "D" ++ show (y+1)
    4 -> "E" ++ show (y+1)
    5 -> "F" ++ show (y+1)
    6 -> "G" ++ show (y+1)
    7 -> "H" ++ show (y+1)
    _ -> "" ++ show (y+1)

humanMove :: Color -> BoardState -> Winner
humanMove humanColor (BoardState whitePawns blackPawns) = if hasOpponentWon then opponentColor else do
    -- let humanPositionChanges = parseHumanInput $ getLine
    Black
    where
        opponentColor = case humanColor of
            Black -> White
            White -> Black
        hasOpponentWon = case opponentColor of
            White -> allFinished whitePawns
            Black -> allFinished blackPawns

isColorMovePossible :: Move -> Color -> BoardState -> Bool
isColorMovePossible mv color (BoardState wp bp) = mv `elem` getAllPossibleColorMoves color allPawns
    where
        allPawns = wp ++ bp

createMove :: [Position] -> Move
createMove ps = Move (head ps) (last ps)

playerPlaysWhite :: BoardState -> IO ()
playerPlaysWhite (BoardState w b) = do
    putStr $ generateBoardRepr (BoardState w b)
    mv <- getLine
    let move = createMove $ parseHumanInput mv
    let movePossible = isColorMovePossible move White (BoardState w b)
    let newWhite = performMove move w
    let newBoard = BoardState newWhite b
    let opMove = opponentMove Black newBoard
    let whiteWon = movePossible && allFinished newWhite 
    let newBlack = performMove opMove b
    let finalBoard = BoardState newWhite newBlack
    let blackWon = allFinished newBlack
    let anyWon = whiteWon || blackWon
    print (if whiteWon then "Bialy wygral" else (if blackWon then "Czarny wygral" else (if movePossible then "Przeciwnik rusza " ++ moveToReadable opMove else "Nieprawidlowy ruch")))
    if anyWon then print "koniec" else if movePossible then playerPlaysWhite finalBoard else playerPlaysWhite (BoardState w b)

playerPlaysBlack :: BoardState -> Bool -> IO ()
playerPlaysBlack (BoardState w b) firstTurn = do
    putStr $ generateBoardRepr (BoardState w b)
    -- postPlayerBlack
    let prePlayerOpMove = opponentMove White $ BoardState w b
    let prePlayerWhite = performMove prePlayerOpMove w
    let currentBoard = if firstTurn then BoardState prePlayerWhite b else BoardState w b
    print $ if firstTurn then "Przeciwnik rusza " ++ moveToReadable prePlayerOpMove else ""
    putStr $ if firstTurn then generateBoardRepr currentBoard else ""
    let prePlayerWhiteWon = allFinished prePlayerWhite 
    mv <- getLine
    let move = createMove $ parseHumanInput mv
    let movePossible = isColorMovePossible move Black currentBoard
    let BoardState currentWhite currentBlack = currentBoard
    let postPlayerBlack = performMove move currentBlack
    let blackWon = movePossible && allFinished postPlayerBlack
    putStr $ generateBoardRepr $ BoardState currentWhite currentBlack
    let opMove = opponentMove White $ BoardState currentWhite postPlayerBlack
    let newWhite = performMove opMove currentWhite
    let finalBoard = BoardState newWhite postPlayerBlack
    let whiteWon = allFinished newWhite
    let anyWon = whiteWon || blackWon
    print (if whiteWon then "Bialy wygral" else (if blackWon then "Czarny wygral" else (if movePossible then "Przeciwnik rusza " ++ moveToReadable opMove else "Nieprawidlowy ruch")))
    if anyWon then print "koniec" else if movePossible then playerPlaysBlack finalBoard False else (if firstTurn then playerPlaysBlack (BoardState prePlayerWhite b) False else playerPlaysBlack (BoardState w b) False)

singlePlayerGame :: IO ()
singlePlayerGame = do
    putStr "Wybierz kolor (X - czarne, O - biale):\n\n"
    line <- getLine 
    let color = parseHumanColor line
    putStr $ generateBoardRepr startingBoard
    case color of
        White -> playerPlaysWhite startingBoard
        Black -> playerPlaysBlack startingBoard True

multiPlayerGame :: IO ()
multiPlayerGame = do
    print 0

determineGameType :: String -> GameType
determineGameType line = case head line of
    'W' -> Multiplayer
    'A' -> Singleplayer

game :: IO ()
game = do
    putStr "Gra wieloosobowa (W) czy przeciwko komputerowi (A):"
    line <- getLine 
    let gameType = determineGameType line
    case gameType of
        Singleplayer -> singlePlayerGame
        Multiplayer -> multiPlayerGame

startingBoard = BoardState [Pawn (Position x y) False White | x <- [0..7], y <- [6..7]] [Pawn (Position x y) False Black | x <- [0..7], y <- [0..1]]
main = game