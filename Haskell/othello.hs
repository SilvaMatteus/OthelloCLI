import Network.CGI.Protocol (maybeRead)
import qualified Data.Map
import Data.Maybe
import Data.List

display_instructions :: IO ()
display_instructions = putStrLn instructions where
    instructions = "\n" ++ line1 ++ line2 ++ line3 ++ line4 ++ line5 ++ line6 ++ line7 ++ line8 ++ line9 ++ line10
    line1 = "---------------------------OthelloCLI v1.0----------------------------\n"
    line2 = "--------------------------------GOAL----------------------------------\n"
    line3 = "----------------------------------------------------------------------\n"
    line4 = "The goal is to have the majority of the markers\n"
    line5 = "in the board at the end of the game.\n"
    line6 = "Each player takes 32 markers and chooses one type (X or O) to\n"
    line7 = "use throughout the game. A move consists of outflanking your\n"
    line8 = "opponent's markers, then flipping the outflanked marks to your marker.\n"
    line9 = "----------------------------------------------------------------------\n"
    line10 = "Enter you move in format (colum, line), Exemple (3, 4) - colum 3, line 4\n"

-- mapping each piece to a position
data Piece = White | Black | Empty deriving (Eq, Show)
type Position = (Int, Int)
type Board = Data.Map.Map Position Piece

-- returns the adversary piece based in my piece
adversaryPiece :: Piece -> Piece
adversaryPiece White = Black
adversaryPiece Black = White
adversaryPiece _ = Empty

-- all pairs represent move direction of players
moveDirections = [(0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1)]

-- mapping initial board positions
generateInitialBoard = (Data.Map.fromList [((3, 3), White), ((4, 4), White), ((3, 4), Black), ((4, 3), Black)])

--generate all matrix cordenates
generateAllPositions = [(x, y) | x <- [0..7], y <- [0..7]]

-- generate empty board
emptyBoard = Data.Map.fromList (zip generateAllPositions (repeat Empty))

--sum positions pairs
sumPosition :: Position -> Position -> Position
sumPosition (positionX1, positionY1) (positionX2, positiony2) = (positionX1 + positionX2, positionY1 + positiony2)

-- verify possible moves using lambda expression and foldr
possibleMoves color board = foldr
    (\position possiblePositions -> if isValidMove color board position then position : possiblePositions else possiblePositions)
    []
    generateAllPositions

-- verify if a movement is valid
isValidMove :: Piece -> Board -> Position -> Bool
isValidMove color board position = (piecesChangedInMove color board position) /= []

-- return list of position of pieces that changed in a move
piecesChangedInMove :: Piece -> Board -> Position -> [Position]
piecesChangedInMove color board position =
    if flipped /= []
        then position : flipped
        else []
    where flipped = concat (map (piecesChangedInRow color board position) moveDirections)

-- verify if is possible to move in a row
verifyRow :: Piece -> Board -> Position -> Position -> Bool
verifyRow color board position direction = verifyRow' True color board position direction

verifyRow' firstLevel color board position direction =
                if nextColor == adversaryPiece color then
                    verifyRow' False color board nextPosition direction
                else
                    nextColor == color && not firstLevel
                where
                    nextPosition = sumPosition position direction
                    nextColor = fromMaybe Empty (Data.Map.lookup nextPosition board)

-- return list of piece that changed in a row
piecesChangedInRow :: Piece -> Board -> Position -> Position -> [Position]
piecesChangedInRow color board position direction = piecesChangedInRow' True color board position direction
  where
    piecesChangedInRow' firstLevel color board position direction =
      if nextColor == adversaryPiece color then
        if restOfRow /= []
          then if not firstLevel
            then position : restOfRow
            else restOfRow
          else []
      else if nextColor == color then
        if not firstLevel
          then [position]
      else []
        else
          []
      where
        nextPosition = sumPosition position direction
        nextColor = fromMaybe Empty (Data.Map.lookup nextPosition board)
        restOfRow = piecesChangedInRow' False color board nextPosition direction

-- calculate number of options avaliable for one move
calculateOptionsAdvantage :: Piece -> Board -> Int
calculateOptionsAdvantage color board = numOptions color board - numOptions (adversaryPiece color) board
  where numOptions color board = sum (map (\position -> if isValidMove color board position then 1 else 0) generateAllPositions)

-- calculate the number of pieces in advantage no generate
calculatePiecesAdvantage :: Piece -> Board -> Int
calculatePiecesAdvantage color board = sum (map
    (\((_, _), x) ->
        if x == color
            then 1
            else if x == adversaryPiece color
                then -1
                else 0)
    (Data.Map.toList board))

-- in all possibile possition and advantages find the best move
findBestMove :: (Piece -> Board -> Int) -> Piece -> Board -> Position
findBestMove advantageFunction color board =
  (\(x, _) -> x)
    (maximumBy
      (\(_, x) (_, y) -> compare x y)
        (map
        (\position -> (position, advantageFunction color (makeMove color board position)))
        generateAllPositions))

-- mensures advantage bases on board state
mensuresAdvantage :: Piece -> Board -> Int
mensuresAdvantage color board = calculatePiecesAdvantage color board + 10 * calculateOptionsAdvantage color board

-- make a move in the board
makeMove :: Piece -> Board -> Position -> Board
makeMove color board position = Data.Map.union (Data.Map.fromList
 (zip (piecesChangedInMove color board position) (repeat color))) board

advantageCalculator :: Int -> Piece -> Board -> Int
advantageCalculator depth color board =
 let
     allValidOpponentMoves = possibleMoves (adversaryPiece color) board
     allValidProponentMoves = possibleMoves color board
     gameOver = allValidProponentMoves == [] && allValidOpponentMoves == []
     in if gameOver
         then
             -- exit condition for game over
             if calculatePiecesAdvantage color board > 0
                 then 1000000
                 else -1000000
         else
             if depth <= 0
                 then
                     -- end of recursion
                  mensuresAdvantage color board
                 else
                     -- calculate opponent move
                     let
                         nextColor = if allValidOpponentMoves /= [] then adversaryPiece color else color
                         validMovesForNextColor = if nextColor /= color then allValidOpponentMoves else allValidProponentMoves
                         maxAdvantageForNextColor =
                             maximum (map
                                 (\position -> advantageCalculator (depth-1) nextColor (makeMove nextColor board position))
                                 validMovesForNextColor)
                     in if nextColor /= color
                         then - maxAdvantageForNextColor
                         else   maxAdvantageForNextColor

colorToPiece :: Piece -> String
colorToPiece color = case color of
  Empty -> " "
  White -> "O"
  Black -> "X"

printBoard :: Board -> String
printBoard board =
  "\n  0 1 2 3 4 5 6 7 \n _________________\n" ++
  (intercalate
   "\n _________________\n"
   (map
      (printRow board)
      [0..7])) ++
   "\n _________________\n  0 1 2 3 4 5 6 7 \n"

printRow :: Board -> Int -> String
printRow board row = show row ++ "|" ++
  (intercalate "|" (map
    (\position -> colorToPiece (fromMaybe Empty (Data.Map.lookup position board)))
    ([(x, row) | x <- [0..7]]))) ++
  "|" ++ show row


playerVsCPUMove color board = do
    -- Verify that none of players can make a move
    let gameOver = possibleMoves color board == [] && possibleMoves (adversaryPiece color) board == []
    if gameOver
        then
            do
                putStr "Game Over\n"
                -- print the winner

                if calculatePiecesAdvantage color board == 0
                    then
                        putStr "Tie"
                    else
                        if calculatePiecesAdvantage color board > 0
                            then putStr ((colorToPiece color) ++ " won !!!!")
                            else putStr ((colorToPiece (adversaryPiece color)) ++ " won !!!!")
        else
            do
                putStr (printBoard board)
                if color == White
                    then
                        -- get move from player
                        do
                            putStr "\n"
                            putStr " Player O Enter your move (column, line): \n"
                            line <- getLine
                            let
                                maybePosition = maybeRead line
                            if isNothing maybePosition || not (isValidMove color board (fromJust maybePosition))
                                then
                                    -- if the move is not valid
                                    do
                                        putStr "Illegal move, Try Again.\n"
                                        playerVsCPUMove color board
                                else
                                    actuallyMove (fromJust maybePosition)
                    else
                        actuallyMove (findBestMove (advantageCalculator 4) color board)
                        -- CPU move
                        where
                            actuallyMove position =
                                -- Make the move
                                do
                                    let
                                        resultingBoard = makeMove color board position
                                        nextColor = if possibleMoves (adversaryPiece color) resultingBoard /= [] then adversaryPiece color else color
                                    playerVsCPUMove nextColor resultingBoard



playerVsPlayerMove color board = do
    -- Verify that none of players can make a move
    let gameOver = possibleMoves color board == [] && possibleMoves (adversaryPiece color) board == []
    if gameOver
        then
            do
                putStr "Game Over\n"
                -- print the winner

                if calculatePiecesAdvantage color board == 0
                    then
                        putStr "Tie"
                    else
                        if calculatePiecesAdvantage color board > 0
                            then putStr ((colorToPiece color) ++ " won !!!!")
                            else putStr ((colorToPiece (adversaryPiece color)) ++ " won !!!!")
        else
            do
                putStr (printBoard board)
                if color == White
                    then
                        -- get move from player )
                        do
                            putStr "\n"
                            putStr "Player O Enter your move (column, line): \n"
                            line <- getLine
                            let
                                maybePosition = maybeRead line
                            if isNothing maybePosition || not (isValidMove color board (fromJust maybePosition))
                                then
                                    -- if the move is not valid
                                    do
                                        putStr "Illegal move, Try Again.\n"
                                        playerVsPlayerMove color board
                                        -- player O move
                                else
                                    actuallyMove (fromJust maybePosition)
                    else
                        do
                            putStr "\n"
                            putStr "Player X Enter your move (column, line): \n"
                            line <- getLine
                            let
                                maybePosition = maybeRead line
                            if isNothing maybePosition || not (isValidMove color board (fromJust maybePosition))
                                then
                                    -- if the move is not valid
                                    do
                                        putStr "Illegal move, Try Again.\n"
                                        playerVsPlayerMove color board
                                        -- Player X move
                                else
                                    actuallyMove (fromJust maybePosition)
                        where
                            actuallyMove position =
                                -- Make the move
                                do
                                    let
                                        resultingBoard = makeMove color board position
                                        nextColor = if possibleMoves (adversaryPiece color) resultingBoard /= [] then adversaryPiece color else color
                                    playerVsPlayerMove nextColor resultingBoard





main = do display_instructions
          putStrLn "\n"
          putStrLn "1- Player vs Player"
          putStrLn "2- Player vs CPU"
          putStrLn "\n"
          putStrLn "Select your Option :"
          option <- getLine
          if option == "1" then playerVsPlayerMove White generateInitialBoard else playerVsCPUMove White generateInitialBoard
