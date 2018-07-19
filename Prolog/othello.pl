start_game() :-   /* main relation, starts the game loop */
	generate_board(Board, 8),
	game_loop(Board, 1, black).

game_loop(Board, Depth, black):- /* overloaded game loops manager */
	print_board(Board),
	print_player(black),
	empty_on_board(Board),
	cpu_select_move(Board, Depth, black, FinalBoard),!,
	game_loop(FinalBoard, Depth, white),!.

game_loop(Board, Depth, white):-   /* To Do some relations */
	print_board(Board),
	print_player(white),
	empty_on_board(Board),
	find_moves(Board, white, MovesList),
	member(_, MovesList),
	player_select_move(Move, MovesList),!,
	set_piece(Board, Move, white, FinalBoard),
	game_loop(FinalBoard, Depth, black),!.

game_loop(Board, _, Color):- /* To Do some relations */
	full_board(Board),
	print_board(Board),
	count_pieces(Color, Board, Pieces, RivalPieces),
	writef('%d: %d\n', [Color, Pieces]),
	adversary_color(Color, RivalColor),
	writef('%d: %d\n', [RivalColor, RivalPieces]),!.

game_loop(Board, Depth, Color):- /* To do some relations */
	find_moves(Board, Color, MovesList),!,
	not(member(_,MovesList)),!,
	print_player(Color),
	writeln('No valid move remaing.'),
	adversary_color(Color, RivalColor),
	game_loop(Board, Depth, RivalColor),!.

print_player(white):-  /* print player prompts */
	writeln('Player O Enter your move:'),!.

print_player(black):-
	writeln('Player X Enter your move:'),!.

player_select_move(Move, MovesList):- /* to Do some relations */
	write('Enter the Row: '),
	read(SelectedRow),
	writeln('Enter the Column: '),
	read(SelectedColum),
	member(Move, MovesList),
	nth0(0, Move, SelectedRow),
	nth0(1, Move, SelectedColum).

player_select_move(Move, MovesList):-
	writeln('Illegal move, Try Again.'),
	writeln(''),
	player_select_move(Move, MovesList).

cpu_select_move(Board, Depth, Color, FinalBoard):-
	pruning(Board, Depth, Color, FinalBoard, _). /* to DO cpu prunning */

adversary_color(white, black).
adversary_color(black, white).

generate_board(Board, 8) :-  /* generate the board */
		Board = [[empty, empty, empty, empty, empty, empty, empty, empty],
		[empty, empty, empty, empty, empty, empty, empty, empty],
		[empty, empty, empty, empty, empty, empty, empty, empty],
		[empty, empty, empty, white, black, empty, empty, empty],
		[empty, empty, empty, black, white, empty, empty, empty],
		[empty, empty, empty, empty, empty, empty, empty, empty],
		[empty, empty, empty, empty, empty, empty, empty, empty],
		[empty, empty, empty, empty, empty, empty, empty, empty]].

print_board(Board) :-
	print_board(Board, 0, 0).

print_board(Board, 7, 8) :-
	writeln('|'),
	writeln(''),!.

print_board(Board, Row, 0) :-
	write('|'),
	piece(Board, Row, 0, Piece),
	print_piece(Piece),
	print_board(Board, Row, 1).

print_board(Board, Row, 8) :-
	Row \= 7,
	NextRow is Row + 1,
	writeln('|,'),
	print_board(Board, NextRow, 0).

print_board(Board, Row, Column) :-
	Column \= 0,
	write(','),
	piece(Board, Row, Column, Piece),
	print_piece(Piece),
	NextColumn is Column + 1,
	print_board(Board, Row, NextColumn).


print_piece(black):-
	write('X').

print_piece(white):-
	write('O').

print_piece(empty):-
	write('-').
