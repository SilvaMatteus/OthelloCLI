CC=gcc
CFLAGS=-I.
CXX_FILES= othello.cpp
DEPS = display_board.hpp

APP_NAME= othello_cli

othello_cli:
	$(CC) $(CXX_FILES) $(DEPS) -o $(APP_NAME)  $< $(CFLAGS)
.PHONY: clean

clean:
	rm -f $(APP_NAME)
