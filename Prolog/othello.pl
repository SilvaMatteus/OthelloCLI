start_game() :-   /* main relation, starts the game loop */
	generate_board(Board, 8),
	game_loop(Board, 1, black).

% some auxiliar relations to manipulate data
first_elements([], BoardsList, BoardsList):-!.

first_elements([First|Rest], Temp, Boards):-
	nth0(0, First, Board),
	append(Temp, [Board], NTemp),
	first_elements(Rest, NTemp, Boards).

min_list([First|Rest], Min):-
	min_list_aux(Rest, First, Min).

min_list_aux([], Min, Min):-!.

min_list_aux([First|Rest], CurrentMin, Min):-
	First < CurrentMin,
	min_list_aux(Rest, First, Min),!.

min_list_aux([_|Rest], CurrentMin, Min):-
	min_list_aux(Rest, CurrentMin, Min),!.

max_list([First|Rest], Max):-
	max_list_aux(Rest, First, Max).

max_list_aux([], Max, Max):-!.

max_list_aux([First|Rest], CurrentMax, Max):-
	First > CurrentMax,
	max_list_aux(Rest, First, Max),!.

max_list_aux([_|Rest], CurrentMax, Max):-
	max_list_aux(Rest, CurrentMax, Max),!.

game_loop(Board, Depth, black):- /* overloaded game loops manager */
	print_board(Board),
	print_player(black),
	empty_on_board(Board),
	cpu_select_move(Board, Depth, black, FinalBoard),!,
	game_loop(FinalBoard, Depth, white),!.

game_loop(Board, Depth, white):-   /* complements game_loop */
	print_board(Board),
	print_player(white),
	empty_on_board(Board),
	find_moves(Board, white, MovesList),
	member(_, MovesList),
	player_select_move(Move, MovesList),!,
	put_piece_in(Board, Move, white, FinalBoard),
	game_loop(FinalBoard, Depth, black),!.

game_loop(Board, _, Color):- /* To Do some relations */
	complete_board(Board),
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

player_select_move(Move, MovesList):- /* processs player move */
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
	pruning(Board, Depth, Color, FinalBoard, _).

adversary_color(white, black).
adversary_color(black, white).

generate_board(Board, 8) :-  /* generate  empty the board */
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
	writeln('|'),
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

piece(Board, RowIndex, ColumnIndex, Piece) :-
	is_valid_index(RowIndex),
	is_valid_index(ColumnIndex),
	nth0(RowIndex, Board, Row),
	nth0(ColumnIndex, Row, Piece).

final(Board, Value):-
	complete_board(Board),
	count_pieces(black, Board, BlackPieces, WhitePieces),
	Value is BlackPieces - WhitePieces.

evaluate(Board, Value):-
	count_pieces(black, Board, BlackPieces, WhitePieces),
	Hvalue1 is BlackPieces - WhitePieces,
	generate_valid_positions(Board, black, BlackValidMoves),
	generate_valid_positions(Board, white, WhiteValidMoves),
	Hvalue2 is BlackValidMoves - WhiteValidMoves,
	max_list([Hvalue1,Hvalue2], Value).

empty_on_board(Board):-
	member(Row, Board),
	member(Piece, Row),
	Piece = empty,!.

complete_board(Board):-
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
	put_piece_in(Board, Move, Color, FinalBoard),
	order_boards(Color, CurrentBoardsList, FinalBoard, NBoardsList),
	search_in_boards(Board, Color, BoardsList, NBoardsList, RestMovesList),!.

order_boards(Color, CurrentBoardsList, FinalBoard, NBoardsList):-
	adversary_color(Color, RivalColor),
	generate_valid_positions(FinalBoard, RivalColor, Number),
	order_boards_2([FinalBoard, Number], CurrentBoardsList, [], NBoardsList).

order_boards_2(Board, [], CurrentList, FinalList):-
	append(CurrentList, [Board], FinalList),!.

order_boards_2(Board, [First|Rest], CurrentList, FinalList):-
	nth0(1, First, Value),
	nth0(1, Board, NewValue),
	NewValue =< Value,
	append(CurrentList, [Board], TempList),
	append(TempList, [First|Rest], FinalList),!.

order_boards_2(Board, [First|Rest], CurrentList, FinalList):-
	append(CurrentList, [First], NCurrentList),
	order_boards_2(Board, Rest, NCurrentList, FinalList),!.

generate_valid_positions(Board, Color, Number):-
	generate_valid_positions(Board, Color, 0, 0, 0, Number).

generate_valid_positions(_, _, 7, 8, Number, Number):-!.

generate_valid_positions(Board, Color, RowIndex, 8, CurrentNumber, FinalNumber):-
	NRowIndex is RowIndex + 1,
	generate_valid_positions(Board, Color, NRowIndex, 0, CurrentNumber, FinalNumber),!.

generate_valid_positions(Board, Color, RowIndex, ColumnIndex, CurrentNumber, FinalNumber):-
	single_valid_move(Board, RowIndex, ColumnIndex, Color),
	NCurrentNumber is CurrentNumber + 1,
	NColumnIndex is ColumnIndex + 1,
	generate_valid_positions(Board, Color, RowIndex, NColumnIndex, NCurrentNumber, FinalNumber),!.

generate_valid_positions(Board, Color, RowIndex, ColumnIndex, CurrentNumber, FinalNumber):-
	NColumnIndex is ColumnIndex + 1,
	generate_valid_positions(Board, Color, RowIndex, NColumnIndex, CurrentNumber, FinalNumber),!.

single_valid_move(Board, RowIndex, ColumnIndex, Color) :-
	piece(Board, RowIndex, ColumnIndex, Piece),
	Piece = empty,
	direction_offsets(DirectionOffsets),
	member(DirectionOffset, DirectionOffsets),
	nth0(0, DirectionOffset, RowOffset),
	nth0(1, DirectionOffset, ColumnOffset),
	NeighborRow is RowIndex + RowOffset,
	NeighborColumn is ColumnIndex + ColumnOffset,
	adversary_color(Color, RivalColor),
	piece(Board, NeighborRow, NeighborColumn, NeighborPiece),
	NeighborPiece = RivalColor,
	find_piece_byColor(Board, NeighborRow, NeighborColumn, RowOffset, ColumnOffset, Color),!.

find_moves(Board, Color, MovesList):-
	find_moves(Board, Color, 0, 0, [], MovesList).

find_moves(_, _, 7, 8, MovesList, MovesList):-!.

find_moves(Board, Color, RowIndex, 8, MovesList, FinalList):-
	NRowIndex is RowIndex + 1,
	find_moves(Board, Color, NRowIndex, 0, MovesList, FinalList),!.

find_moves(Board, Color, RowIndex, ColumnIndex, MovesList, FinalList):-
	check_valid_move(Board, RowIndex, ColumnIndex, Color, ValidDirectionOffsets),
	append(MovesList,[[RowIndex, ColumnIndex, ValidDirectionOffsets]], NMovesList),
	NColumnIndex is ColumnIndex + 1,
	find_moves(Board, Color, RowIndex, NColumnIndex, NMovesList, FinalList),!.

find_moves(Board, Color, RowIndex, ColumnIndex, MovesList, FinalList):-
	NColumnIndex is ColumnIndex + 1,
	find_moves(Board, Color, RowIndex, NColumnIndex, MovesList, FinalList),!.

check_valid_move(Board, RowIndex, ColumnIndex, Color, ValidDirectionOffsets):-
	piece(Board, RowIndex, ColumnIndex, Piece),
	Piece = empty,
	direction_offsets(DirectionOffsets),
	check_valid_move(Board, RowIndex, ColumnIndex, Color, DirectionOffsets, [], ValidDirectionOffsets).

check_valid_move(_, _, _, _, [], CurrentValidDirectionOffsets, ValidDirectionOffsets):-
	CurrentValidDirectionOffsets \= [],
	CurrentValidDirectionOffsets = ValidDirectionOffsets.

check_valid_move(Board, RowIndex, ColumnIndex, Color, DirectionOffsets, CurrentValidDirectionOffsets, ValidDirectionOffsets):-
	DirectionOffsets = [DirectionOffset|DirectionOffsetsRest],
	valid_move_offset(Board, RowIndex, ColumnIndex, Color, DirectionOffset),
	append(CurrentValidDirectionOffsets, [DirectionOffset], NCurrentValidDirectionOffsets),
	check_valid_move(Board, RowIndex, ColumnIndex, Color, DirectionOffsetsRest, NCurrentValidDirectionOffsets, ValidDirectionOffsets).

check_valid_move(Board, RowIndex, ColumnIndex, Color, DirectionOffsets, CurrentValidDirectionOffsets, ValidDirectionOffsets):-
	DirectionOffsets = [_|DirectionOffsetsRest],
	check_valid_move(Board, RowIndex, ColumnIndex, Color, DirectionOffsetsRest, CurrentValidDirectionOffsets, ValidDirectionOffsets).

valid_move_offset(Board, RowIndex, ColumnIndex, Color, DirectionOffset):-
	piece(Board, RowIndex, ColumnIndex, Piece),
	Piece = empty,
	nth0(0, DirectionOffset, RowOffset),
	nth0(1, DirectionOffset, ColumnOffset),
	NeighborRow is RowIndex + RowOffset,
	NeighborColumn is ColumnIndex + ColumnOffset,
	adversary_color(Color, RivalColor),
	piece(Board, NeighborRow, NeighborColumn, NeighborPiece),
	NeighborPiece = RivalColor,
	find_piece_byColor(Board, NeighborRow, NeighborColumn, RowOffset, ColumnOffset, Color).

find_piece_byColor(Board, RowIndex, ColumnIndex, RowOffset, ColumnOffset, Color) :-
	NRowOffset is RowIndex + RowOffset,
	NColumnOffset is ColumnIndex + ColumnOffset,
	piece(Board, NRowOffset, NColumnOffset, Piece),
	Piece = Color.

find_piece_byColor(Board, RowIndex, ColumnIndex, RowOffset, ColumnOffset, Color) :-
	NRowIndex is RowIndex + RowOffset,
	NColumnIndex is ColumnIndex + ColumnOffset,
	piece(Board, NRowIndex, NColumnIndex, Piece),
	adversary_color(Color, RivalColor),
	Piece = RivalColor,
	find_piece_byColor(Board, NRowIndex, NColumnIndex, RowOffset, ColumnOffset, Color).

put_piece_in(Board, Move, Color, FinalBoard):-
	nth0(0, Move, Row),
	nth0(1, Move, Column),
 	nth0(2, Move, ValidDirectionOffsets),
	set_single_piece(Board, Row, Column, Color, BoardWithPiece),
	put_pieces_on_offsets(BoardWithPiece, Row, Column, Color, ValidDirectionOffsets, FinalBoard).

put_piece_in(Board, PieceRowIndex, PieceColumnIndex, Color, FinalBoard):-
	check_valid_move(Board, PieceRowIndex, PieceColumnIndex, Color, ValidDirectionOffsets),
	set_single_piece(Board, PieceRowIndex, PieceColumnIndex, Color, BoardWithPiece),
	put_pieces_on_offsets(BoardWithPiece, PieceRowIndex, PieceColumnIndex, Color, ValidDirectionOffsets, FinalBoard).

put_pieces_on_offsets(FinalBoard, _, _, _, [], FinalBoard):-!.

put_pieces_on_offsets(Board, PieceRowIndex, PieceColumnIndex, Color, ValidDirectionOffsets, FinalBoard):-
	ValidDirectionOffsets = [ValidDirectionOffset|ValidDirectionOffsetsRest],
	put_pieces_on_offset(Board, PieceRowIndex, PieceColumnIndex, Color, ValidDirectionOffset, TempBoard),
	put_pieces_on_offsets(TempBoard, PieceRowIndex, PieceColumnIndex, Color, ValidDirectionOffsetsRest, FinalBoard).

put_pieces_on_offset(Board, PieceRowIndex, PieceColumnIndex, Color, ValidDirectionOffset, FinalBoard):-
	nth0(0, ValidDirectionOffset, RowOffset),
	nth0(1, ValidDirectionOffset, ColumnOffset),
	NRowOffset is PieceRowIndex + RowOffset,
	NColumnOffset is PieceColumnIndex + ColumnOffset,
	piece(Board, NRowOffset, NColumnOffset, Piece),
	Piece = Color,
	Board = FinalBoard,!.

put_pieces_on_offset(Board, PieceRowIndex, PieceColumnIndex, Color, ValidDirectionOffset, FinalBoard):-
	nth0(0, ValidDirectionOffset, RowOffset),
	nth0(1, ValidDirectionOffset, ColumnOffset),
	NRowOffset is PieceRowIndex + RowOffset,
	NColumnOffset is PieceColumnIndex + ColumnOffset,
	piece(Board, NRowOffset, NColumnOffset, Piece),
	adversary_color(Color, RivalColor),
	Piece = RivalColor,
	set_single_piece(Board, NRowOffset, NColumnOffset, Color, TempBoard),
	put_pieces_on_offset(TempBoard, NRowOffset, NColumnOffset, Color, ValidDirectionOffset, FinalBoard).

set_single_piece(Board, PieceRowIndex, PieceColumnIndex, Color, FinalBoard):-
	set_single_piece(Board, PieceRowIndex, PieceColumnIndex, 0, 0, Color, [], FinalBoard, []).

set_single_piece(_, 7, _, 7, 8, _, ResultingBoard, FinalBoard, PieceRow):-
	append(ResultingBoard, [PieceRow], FinalBoard),!.

set_single_piece(_, _, _, 8, 0, _, FinalBoard, FinalBoard, _):-!.

set_single_piece(Board, PieceRowIndex, ColumnRowIndex, PieceRowIndex, 8, Color, ResultingBoard, FinalBoard, RowIndex):-
	PieceRowIndex \= 7,
	NCurrentRowIndex is PieceRowIndex + 1,
	append(ResultingBoard, [RowIndex], NResultingBoard),
	set_single_piece(Board, PieceRowIndex, ColumnRowIndex, NCurrentRowIndex, 0, Color, NResultingBoard, FinalBoard, []).

set_single_piece(Board, PieceRowIndex, PieceColumnIndex, PieceRowIndex, PieceColumnIndex, Color, ResultingBoard, FinalBoard, PieceRow):-
	append(PieceRow, [Color], NPieceRow),
	NCurrentColumnIndex is PieceColumnIndex + 1,
	set_single_piece(Board, PieceRowIndex, PieceColumnIndex, PieceRowIndex, NCurrentColumnIndex, Color, ResultingBoard, FinalBoard, NPieceRow).

set_single_piece(Board, PieceRowIndex, PieceColumnIndex, PieceRowIndex, CurrentColumnIndex, Color, ResultingBoard, FinalBoard, PieceRow):-
	CurrentColumnIndex \= PieceColumnIndex,
	piece(Board, PieceRowIndex, CurrentColumnIndex, Piece),
	append(PieceRow, [Piece], NPieceRow),
	NCurrentColumnIndex is CurrentColumnIndex + 1,
	set_single_piece(Board, PieceRowIndex, PieceColumnIndex, PieceRowIndex, NCurrentColumnIndex, Color, ResultingBoard, FinalBoard, NPieceRow).

set_single_piece(Board, PieceRowIndex, PieceColumnIndex, CurrentRowIndex, _, Color, ResultingBoard, FinalBoard, PieceRow):-
	PieceRowIndex \= CurrentRowIndex,
	nth0(CurrentRowIndex, Board, CurrentRow),
	append(ResultingBoard, [CurrentRow], NResultingBoard),
	NCurrentRowIndex is CurrentRowIndex + 1,
	set_single_piece(Board, PieceRowIndex, PieceColumnIndex, NCurrentRowIndex, 0, Color, NResultingBoard, FinalBoard, PieceRow).

count_pieces(Color, Board, Pieces, RivalPieces) :-
	count_pieces(Color, Board, 0, 0, 0, 0, Pieces, RivalPieces).

count_pieces(_, _, 7, 8, CurrentPieces, CurrentRivalPieces, Pieces, RivalPieces):-
	Pieces is CurrentPieces,
	RivalPieces is CurrentRivalPieces,!.

count_pieces(Color, Board, RowIndex, 8, CurrentPieces, CurrentRivalPieces, Pieces, RivalPieces) :-
	NRowIndex is RowIndex + 1,
	count_pieces(Color, Board, NRowIndex, 0, CurrentPieces, CurrentRivalPieces, Pieces, RivalPieces).

count_pieces(Color, Board, RowIndex, ColumnIndex, CurrentPieces, CurrentRivalPieces, Pieces, RivalPieces) :-
	piece(Board, RowIndex, ColumnIndex, Piece),
	count_piece(Color, Piece, CurrentPieces, CurrentRivalPieces, NCurrentPieces, NCurrentRivalPieces),
	NColumnIndex is ColumnIndex + 1,
	count_pieces(Color, Board, RowIndex, NColumnIndex, NCurrentPieces, NCurrentRivalPieces, Pieces, RivalPieces).

count_piece(_, empty, CurrentPieces, CurrentRivalPieces, CurrentPieces, CurrentRivalPieces):-!.

count_piece(Color, Color, CurrentPieces, CurrentRivalPieces, NCurrentPieces, CurrentRivalPieces):-
	NCurrentPieces is CurrentPieces + 1,!.

count_piece(Color, RivalColor, CurrentPieces, CurrentRivalPieces, CurrentPieces, NCurrentRivalPieces):-
	adversary_color(Color, RivalColor),
	NCurrentRivalPieces is CurrentRivalPieces + 1,!.

direction_offsets(OffsetsList) :-
	OffsetsList = [[-1, 0],
			[-1, 1],
			[0, 1],
			[1, 1],
			[1, 0],
			[1, -1],
			[0, -1],
			[-1,-1]].

is_valid_index(Index) :-
	Index >= 0,
	Index < 8.

pruning(State, Depth, Color, NewState, Value):-
	pruning(Depth, State, Color, NewState, Value, -1000, 1000).

pruning(_, State, _, State, Value, _, _) :- final(State, Value),!.

pruning(0, State, _, State, Value, _, _) :- evaluate(State, Value),!.

pruning(Depth, State, Color, NewState, Value, Alpha, Beta) :-
	Depth > 0,
	search_states(State, Color, StatesList),
	adversary_color(Color, RivalColor),
	NDepth is Depth - 1,
	catch(
		pruning(StatesList, NDepth, Color, RivalColor, NewState, Value, Alpha, Beta),
		_,
		pruning_search(StatesList, Depth, Color, RivalColor, NewState, Value, Alpha, Beta)).

pruning_search(StatesList, Depth, Color, RivalColor, NewState, Value, Alpha, Beta):-

	writef('Search at depth %d\n', [Depth]),
	pruning(StatesList, 0, Color, RivalColor, NewState, Value, Alpha, Beta).

pruning([State], Depth, _, RivalColor, State, Value, Alpha, Beta):- !,
	pruning(Depth, State, RivalColor, _, Value, Alpha, Beta).

pruning([State|Rest], Depth, Color, RivalColor, NewState, Value, Alpha, Beta) :-
	pruning(Depth, State, RivalColor, _, X, Alpha, Beta),
	(
		prune(Color, X, Alpha, Beta) ->
		(
			NewState = State,
			Value is X
		);
		(
			recalc(Color, X, Alpha, Beta, Nalpha, NBeta),
			pruning(Rest, Depth, Color, RivalColor, B, Y, Nalpha, NBeta),
			best(Color, X, Y, State, B, NewState, Value)
		)

	).

prune(black, Value, _, Beta):-
	Value >= Beta.

prune(white, Value, Alpha, _):-
	Value =< Alpha.

recalc(black, Value, Alpha, Beta, Nalpha, Beta):-
	max_list([Alpha, Value], Nalpha).

recalc(white, Value, Alpha, Beta, Alpha, NBeta):-
	min_list([Beta, Value], NBeta).

best(black, X, Y, A, _, A, X):- X>=Y,!.
best(black, _, Y, _, B, B, Y).
best(white, X, Y, A, _, A, X):- X=<Y, !.
best(white, _, Y, _, B, B, Y).
