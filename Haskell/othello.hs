import qualified displayBoard as display

MAX_LINE_INDEX = 7

MAX_COLUMN_INDEX = 7

ERROR_INVALID_POSITION = -1

NO_MOVEMENT_REMAINING = 0

ERROR_INVALID_MOVE = -4

PLAYER_X = 'X'

board_map = [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
            [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
            [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
            [' ', ' ', ' ', 'X', 'O', ' ', ' ', ' '],
            [' ', ' ', ' ', 'O', 'X', ' ', ' ', ' '],
            [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
            [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
            [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ']


movements_remaining = ( ( MAX_LINE_INDEX + 1 )  * ( MAX_COLUMN_INDEX  + 1) ) - 4
