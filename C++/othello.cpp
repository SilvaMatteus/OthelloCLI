#include <stdio.h>
#include <unistd.h>
#include <cstdlib>
#include "display_board.hpp"

#define MAX_LINE_INDEX               7
#define MAX_COLUMN_INDEX             7
#define ERROR_INVALID_POSITION      -1
#define ERROR_NO_MOVEMENT_REMAINING -2

#define SAFE_FREE( ptr ) { if ( ( NULL !=  ptr )  && ( nullptr != ptr ) ) { free( ptr ); ( ptr ) = NULL; } }

// The board map is configured with the initial position
char board_map[8][8] = {
    {' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '},
    {' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '},
    {' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '},
    {' ', ' ', ' ', 'X', 'O', ' ', ' ', ' '},
    {' ', ' ', ' ', 'O', 'X', ' ', ' ', ' '},
    {' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '},
    {' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '},
    {' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '}
};

// Count possible moves efficiently
int movements_remaining = ( 8 * 8 ) - 4;

void get_movement_stdin( int *p_line, int *p_column )
{
    scanf("%d %d", p_line, p_column);
}

int make_move( int move_line, int move_column ) {
    
    if ( move_line < 0 || move_column < 0 || move_line > 7 || move_line > 0 )
        return ERROR_INVALID_POSITION;
    if ( movements_remaining == 0 )
        return ERROR_NO_MOVEMENT_REMAINING;
}

void start_game_loop() {
    while ( true ) {
        update_display_board( board_map );
        print_display_board();

        // Starts a new turn.
        int move_line, move_column;
        get_movement_stdin( &move_line, &move_column );

        int result = make_move( move_line, move_column );

#ifdef DEBUG_MODE
        printf("\n>> Movement: [ %d, %d ].\n", move_line, move_column);
        sleep(2);
#endif
    }
}


int main() {
    // update_display_board(test);
    // print_display_board();

    start_game_loop();
}