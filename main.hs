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
getAllPossibleColorMoves color1 pawns = concatMap (`getPossiblePawnMoves` pawns) (filter (\(Pawn _ finished color2) -> not finished && color1 == color2) pawns)

getMoveGreedyValue :: Move -> Int
getMoveGreedyValue (Move (Position x1 y1) (Position x2 y2)) = y2 - y1

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
opponentMove color (BoardState whitePawns blackPawns) = head $ filter (\move -> getMoveGreedyValue move == max_value) possible_moves
    where
        possible_moves = getAllPossibleColorMoves color (whitePawns++blackPawns)
        max_value = maximum $ map getMoveGreedyValue possible_moves

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

isMovePossible :: Move -> BoardState -> Bool
isMovePossible mv bs = getAllPossibleColorMoves

pawnsBlack = [Pawn (Position i j) False Black | i <- [0..7], j <- [0..1]]
pawnsWhite= [Pawn (Position i j) False White | i <- [0..7], j <- [6..7]]
pawns = pawnsBlack ++ pawnsWhite
pawnsNew = performMove (Move (Position 0 1) (Position 0 2)) pawns

playerPlaysWhite :: BoardState -> IO ()
playerPlaysWhite (BoardState w b) = do
    putStr $ generateBoardRepr (BoardState w b)
    mv <- getLine
    let move = parseHumanInput mv
    print move

playerPlaysBlack :: BoardState -> IO ()
playerPlaysBlack board = do
    print 4

singlePlayerGame = do
    putStr "Wybierz kolor (X - czarne, O - biale):\n\n"
    line <- getLine 
    let color = parseHumanColor line
    putStr $ generateBoardRepr pawns
    case color of
        White -> playerPlaysWhite pawns
        Black -> playerPlaysBlack pawns

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

color = "xx"
main = do
    game
