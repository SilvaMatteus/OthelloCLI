#include <stdio.h>

#define WHITE_PIECE  'O'
#define BLACK_PIECE  'X'
#define LINE_SIZE      8
#define COLUMN_SIZE    8

int main() {
    for ( int i = 0; i <= LINE_SIZE; i++ ) {
        for ( int j = 0; j <= COLUMN_SIZE; j++ ) {
            if ( i % 2 == 0 ) {
                printf( " %c ", BLACK_PIECE );
            } else {
                printf( " %c ", WHITE_PIECE );
            }
            
        }
        printf( "\n" );
    }
}