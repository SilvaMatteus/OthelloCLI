import Network.CGI.Protocol (maybeRead)
import qualified Data.Map
import Data.Maybe
import Data.List


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

calculatePiecesAdvantage :: Piece -> Board -> Int
calculatePiecesAdvantage color board = sum (map
    (\((_, _), x) ->
        if x == color
            then 1
            else if x == adversaryPiece color
                then -1
                else 0)
    (Data.Map.toList board))

main = return ()
