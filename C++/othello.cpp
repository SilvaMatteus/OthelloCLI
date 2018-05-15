#include <stdio.h>
#include <unistd.h>
#include <cstdlib>
#include "display_board.hpp"

#define MAX_LINE_INDEX               7
#define MAX_COLUMN_INDEX             7
#define ERROR_INVALID_POSITION      -1
#define NO_MOVEMENT_REMAINING        0
#define ERROR_INVALID_MOVE          -4

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

// Define the initial turn.
bool is_x_turn = true;

// Count possible moves efficiently
int movements_remaining = ( 8 * 8 ) - 4;

void get_movement_stdin( int *p_line, int *p_column )
{
    if ( is_x_turn )
        printf( "Player X: enter your next move: \n" );
    else
        printf( "Player O: enter your next move: \n" );
    scanf("%d %d", p_line, p_column);
}

int check_line( int move_line,
                int move_column,
                char player_cell,
                char adversary_cell ) {
    // TODO: Implement!
    return 0;
}

int check_column( int move_line,
                int move_column,
                char player_cell,
                char adversary_cell ) {
    // TODO: Implement!
    return 0;
}

int check_main_diagonal( int move_line,
                int move_column,
                char player_cell,
                char adversary_cell ) {
    // TODO: Implement!
    return 0;
}

int check_secondary_diagonal( int move_line,
                int move_column,
                char player_cell,
                char adversary_cell ) {
    // TODO: Implement!
    return 0;
}

int make_move( int move_line, int move_column ) {
    
    if ( move_line < 0 || move_column < 0 || move_line > 7 || move_line > 0 )
        return ERROR_INVALID_POSITION;

    char player_cell = 'O';
    char adversary_cell = 'X';
    if ( is_x_turn ) {
        player_cell = 'X';
        adversary_cell = 'O';
    }

    // The methods bellow modify the board.
    // They must return -1 if there is no valid move.
    int error_status = 0;
    error_status += check_line( move_line, move_column, player_cell, adversary_cell );
    error_status += check_column( move_line, move_column, player_cell, adversary_cell );
    error_status += check_main_diagonal( move_line, move_column, player_cell, adversary_cell );
    error_status += check_secondary_diagonal( move_line, move_column, player_cell, adversary_cell );
    if ( error_status == ERROR_INVALID_MOVE )
        return ERROR_INVALID_MOVE;
}

char check_winner() {
    int x_count = 0;
    int o_count = 0;
    for ( int i = 0; i < 8; i++ ) {
        for ( int j = 0; j < 8; j++ ) {
            if ( board_map[i][j] == 'X' )
                x_count++;
            else
                o_count++;
        }
    }
    if ( x_count > o_count )
        return 'X';
    else
        return 'O';
}

void start_game_loop() {
    char winner = ' ';
    while ( true ) {
        update_display_board( board_map );
        print_display_board();

        // Starts a new turn.
        int move_line, move_column;
        get_movement_stdin( &move_line, &move_column );

        int result = make_move( move_line, move_column );
        // If there is no move, count the cells to announce the winner.
        if ( movements_remaining == NO_MOVEMENT_REMAINING ) {
            winner = check_winner();
            break;
        }
        is_x_turn = !is_x_turn; // Change the current player.

#ifdef DEBUG_MODE
        printf("\n>> Movement: [ %d, %d ].\n", move_line, move_column);
        sleep(2);
#endif
    }
    display_victory_message();
}


int main() {
    // update_display_board(test);
    // print_display_board();

    start_game_loop();
}