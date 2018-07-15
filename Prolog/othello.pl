play(Depth, Size) :-
	board_build(Board, Size),
	game_loop(Board, Depth, black).

game_loop(Board, Depth, black):-
	print_board(Board),
	print_player(black),
	empty_on_board(Board),
	machine_select_move(Board, Depth, black, FinalBoard),!,
	game_loop(FinalBoard, Depth, white),!.

game_loop(Board, Depth, white):-
	print_board(Board),
	print_player(white),
	empty_on_board(Board),
	find_moves(Board, white, MovesList),
	member(_, MovesList),
	human_select_move(Move, MovesList),!,
	set_piece(Board, Move, white, FinalBoard),
	game_loop(FinalBoard, Depth, black),!.

game_loop(Board, _, Color):-
	full_board(Board),
	print_board(Board),
	count_pieces(Color, Board, Pieces, RivalPieces),
	writef('%d: %d\n', [Color, Pieces]),
	rival_color(Color, RivalColor),
	writef('%d: %d\n', [RivalColor, RivalPieces]),!.

game_loop(Board, Depth, Color):-
	find_moves(Board, Color, MovesList),!,
	not(member(_,MovesList)),!,
	print_player(Color),
	writeln('There\'s no valid move'),
	rival_color(Color, RivalColor),
	game_loop(Board, Depth, RivalColor),!.
