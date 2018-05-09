CC=gcc
CFLAGS= -std=c++11 -I.
CXX_FILES= othello.cpp
DEPS = display_board.hpp
DEBUG ?= 0

ifeq ($(DEBUG), 1)
	CFLAGS += -DDEBUG_MODE
endif

APP_NAME= othello_cli

othello_cli:
	$(CC) $(CXX_FILES) $(DEPS) -o $(APP_NAME)  $< $(CFLAGS)

.PHONY: clean

clean:
	rm -f $(APP_NAME)
