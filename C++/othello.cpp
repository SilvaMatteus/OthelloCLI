#include <stdio.h>
#include <unistd.h>
#include <cstdlib>
#include "display_board.hpp"

#define MAX_LINE_INDEX      7
#define MAX_COLUMN_INDEX    7

#define SAFE_FREE( ptr ) { if ( ( NULL !=  ptr )  && ( nullptr != ptr ) ) { free( ptr ); ( ptr ) = NULL; } }

char board_map[8][8] = {
    {' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '},
    {' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '},
    {' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '},
    {' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '},
    {' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '},
    {' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '},
    {' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '},
    {' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '}
};


void start_game_loop() {
    while ( true ) {
        update_display_board( board_map );
        print_display_board();
        char *p_move = ( char * ) calloc( 3, 1 );
        scanf( "%s", p_move );
#ifdef DEBUG_MODE
        printf("\n<< %s >>\n", p_move);
        sleep(2);
#endif
        SAFE_FREE( p_move );
    }
}


int main() {
    // update_display_board(test);
    // print_display_board();

    start_game_loop();
}