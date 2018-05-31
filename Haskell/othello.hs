import qualified DisplayBoard as D

max_LINE_INDEX = 7

max_COLUMN_INDEX = 7

error_INVALID_POSITION = -1

no_MOVEMENT_REMAINING = 0

error_INVALID_MOVE = -4

player_X = 'X'
player_O = 'O'

board_map = [[' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
            [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
            [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
            [' ', ' ', ' ', 'X', 'O', ' ', ' ', ' '],
            [' ', ' ', ' ', 'O', 'X', ' ', ' ', ' '],
            [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
            [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
            [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ']]


movements_remaining = ( ( max_LINE_INDEX + 1 ) * ( max_COLUMN_INDEX  + 1) ) - 4

main :: IO ()
main = return ()
