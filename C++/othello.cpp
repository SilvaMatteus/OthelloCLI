#include <stdio.h>
#include "display_board.hpp"

#define MAX_LINE_INDEX      7
#define MAX_COLUMN_INDEX    7

int main() {
    char test[8][8] = {
        {'X', ' ', ' ', ' ', ' ', ' ', ' ', ' '},
        {' ', ' ', ' ', ' ', ' ', ' ', 'X', ' '},
        {' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '},
        {' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '},
        {' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '},
        {' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '},
        {' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '},
        {' ', ' ', ' ', 'X', ' ', ' ', ' ', ' '}
    };
    update_display_board(test);
    print_display_board();
}