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

main = return ()
