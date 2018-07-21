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

print_board(Board) :-  /* some print board methods relations */
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


print_piece(black):- /* piece relations */
	write('X').

print_piece(white):-
	write('O').

print_piece(empty):-
	write('-').

piece(Board, RowIndex, ColumnIndex, Piece) :- /* piece validation */
	is_valid_index(RowIndex),
	is_valid_index(ColumnIndex),
	nth0(RowIndex, Board, Row),
	nth0(ColumnIndex, Row, Piece).

final(Board, Value):-
	complete_board(Board),
	count_pieces(black, Board, BlackPieces, WhitePieces),
	Value is BlackPieces - WhitePieces.

evaluate(Board, Value):- /* evaluate board relations */
	count_pieces(black, Board, BlackPieces, WhitePieces),
	Hvalue1 is BlackPieces - WhitePieces,
	valid_positions(Board, black, BlackValidMoves),
	valid_positions(Board, white, WhiteValidMoves),
	Hvalue2 is BlackValidMoves - WhiteValidMoves,
	max_list([Hvalue1,Hvalue2], Value).

empty_on_board(Board):-
	member(Row, Board),
	member(Piece, Row),
	Piece = empty,!.

complete_board(Board):- /* search in board relations */
	flatten(Board, PiecesList),
	list_to_set(PiecesList, PiecesSet),
	not(member(empty, PiecesSet)).

complete_board(Board, Color, Value):-
	flatten(Board, PiecesList),
	list_to_set(PiecesList, PiecesSet),
	not(member(empty, PiecesSet)),
	count_pieces(Color, Board, Pieces, RivalPieces),
	Value is Pieces - RivalPieces.

search_states(State, Color, StatesList):-
	search_in_boards(State, Color, StatesList).
	search_in_boards(Board, Color, BoardsList):-
	find_moves(Board, Color, MovesList),
	search_in_boards(Board, Color, OrderedBoardsList, [], MovesList),
	first_elements(OrderedBoardsList, [], BoardsList).

search_in_boards(Board,_, BoardsList, [], []):-
	append([], [[Board, 0]], BoardsList),!.

search_in_boards(_, _, BoardsList, BoardsList, []):-!.

search_in_boards(Board, Color, BoardsList, CurrentBoardsList, [Move|RestMovesList]):-
	set_piece(Board, Move, Color, FinalBoard),
	order_boards(Color, CurrentBoardsList, FinalBoard, NBoardsList),
	search_in_boards(Board, Color, BoardsList, NBoardsList, RestMovesList),!.
