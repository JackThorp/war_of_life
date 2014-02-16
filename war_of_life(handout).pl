
% version for SICStus 4.x

:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(random)).

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% PLAYING THE GAME: BASIC CONTROL

%%%%% play/5

play(ShowFlag, FirstPlayerStrategy, SecondPlayerStrategy, TotalMoves, Winner) :-
 start_config(random, Board),
 (
  ShowFlag == verbose,
  format('~nInitial State:~n~n', []),
  draw_board(Board),
  show_score(verbose, Board)
  ;
  ShowFlag == quiet
 ),
 !,
 make_move(Board, ShowFlag, _, 'b', FirstPlayerStrategy, 'r', SecondPlayerStrategy, 0, TotalMoves, Winner).

%%%%% make_move/10
%
% Arguments are as follows:
%
% make_move(Board, ShowBoard, FinalBoard, Player, PlayerStrat, NextPlayer, NextPlayerStrat, Moves, TotalMoves, Winner)

make_move([[],[]], ShowFlag, [[],[]], _, _, _, _, NumMoves, NumMoves, 'draw') :-
 !,
 show_winner(ShowFlag, 'draw', NumMoves).

make_move(_, ShowFlag, _, _, _, _, _, 250, 250, 'exhaust') :-
 !,
 show_winner(ShowFlag, 'exhaust', 250).

make_move([[],Reds], ShowFlag, [[],Reds], _, _, _, _, NumMoves, NumMoves, 'r') :-
 !,
 show_winner(ShowFlag, 'red', NumMoves).

make_move([Blues,[]], ShowFlag, [Blues,[]], _, _, _, _, NumMoves, NumMoves, 'b') :-
 !,
 show_winner(ShowFlag, 'blue', NumMoves).

make_move(Board, ShowFlag, FinalBoard, Player, Strategy, NextPlayer, NextStrategy, NumMoves, TotalMoves, Winner) :-
 NewNumMoves is NumMoves + 1,
 move_piece(Player, Strategy, Board, NewBoard, Move),
 show_move(ShowFlag, NewNumMoves, Player, Move),
 draw_board(ShowFlag, NewBoard),
 next_generation(NewBoard, CrankedNewBoard),
 draw_board(ShowFlag, CrankedNewBoard),
 show_score(ShowFlag, CrankedNewBoard),
 !,
 make_move(CrankedNewBoard, ShowFlag, FinalBoard, NextPlayer, NextStrategy, Player, Strategy, NewNumMoves, TotalMoves, Winner).

make_move(_, ShowFlag, _, _, _, _, _, TotalMoves, TotalMoves, 'stalemate') :-
  show_winner(ShowFlag, 'Stalemate', TotalMoves).

%%%%% alter_board/3
%
% replaces a pair [A,B] with [MA,MB] in Alives; result is NewAlives
% Alives must be ordered; NewAlives will be too.

alter_board([A,B,MA,MB], Alives, NewAlives) :-
 ord_del_element(Alives, [A,B], AlivesMinus),
 ord_add_element(AlivesMinus, [MA,MB], NewAlives).

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% RANDOM MOVE STRATEGY

random_move(Alive, OtherPlayerAlive, Move) :-
 findall([A,B,MA,MB],(member([A,B], Alive),
                      neighbour_position(A,B,[MA,MB]),
	              \+member([MA,MB],Alive),
	              \+member([MA,MB],OtherPlayerAlive)),
	 PossMoves),
 length(PossMoves,L),
 LP1 is L + 1,
 random(1, LP1, Pos),
 nth1(Pos, PossMoves, Move).

move_piece('b', random, [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
 random_move(AliveBlues, AliveReds, Move),
 alter_board(Move, AliveBlues, NewAliveBlues).

move_piece('r', random, [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
 random_move(AliveReds, AliveBlues, Move),
 alter_board(Move, AliveReds, NewAliveReds).

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% SUPPORT FOR OTHER STRATEGIES

move_piece(PieceColour, bloodlust, Board, NewBoard, Move) :-
 bloodlust(PieceColour, Board, NewBoard, Move).

move_piece(PieceColour, self_preservation, Board, NewBoard, Move) :-
 self_preservation(PieceColour, Board, NewBoard, Move).

move_piece(PieceColour, land_grab, Board, NewBoard, Move) :-
 land_grab(PieceColour, Board, NewBoard, Move).

move_piece(PieceColour, safety_in_numbers, Board, NewBoard, Move) :-
 safety_in_numbers(PieceColour, Board, NewBoard, Move).

move_piece(PieceColour, minimax, Board, NewBoard, Move) :-
 minimax(PieceColour, Board, NewBoard, Move).

move_piece(PieceColour, divide_and_conquer, Board, NewBoard, Move) :-
 divide_and_conquer(PieceColour, Board, NewBoard, Move).

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% STARTING CONFIGURATIONS

%%%%% start_config/2

start_config(random, [OrdBlues,OrdReds]) :-
 !,
 findall([R,C], cell(R,C), Cells),
 pick(12, Cells, Blues, Rest),
 pick(12, Rest, Reds, _),
 list_to_ord_set(Blues, OrdBlues),
 list_to_ord_set(Reds, OrdReds).

start_config(cross, [[[1,1],[2,2],[3,3],[4,4],[5,5],[6,6],[7,7],[8,8]],
		     [[1,8],[2,7],[3,6],[4,5],[5,4],[6,3],[7,2],[8,1]]]) :-
 !.

start_config(checkers, [[[3,1],[4,2],[3,3],[4,4],[3,5],[4,6],[3,7],[4,8]],
			[[5,1],[6,2],[5,3],[6,4],[5,5],[6,6],[5,7],[6,8]]]) :-
 !.

start_config(gliders, [[[2,1],[3,2],[3,3],[1,3],[2,3]], 
		       [[6,6],[6,7],[6,8],[7,6],[8,7]]]) :-
 !.

start_config(X,X) :-
 ground(X).

%%%%% cell/2
%
% backtracks to find all cells

cell(A, B) :-
 member(A, [1,2,3,4,5,6,7,8]),
 member(B, [1,2,3,4,5,6,7,8]).

%%%%% pick/4

pick(Total, From, Picked, Rest) :-
 pick_aux(0, Total, From, Picked, Rest).

%%%%% pick_aux/5

pick_aux(Total, Total, Rest, [], Rest) :-
 !.

pick_aux(N, Total, From, [E|Picked], Rest) :-
 random_select(E, From, NewFrom),
 N1 is N + 1,
 pick_aux(N1, Total, NewFrom, Picked, Rest).

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% CONWAY CRANK (NEXT GENERATION)

%%%%% next_generation/2
%
% basc control for Conway next generation

next_generation(Board, [NewAliveBlues, NewAliveReds]) :-
 findall([A,B,NewW], (cell(A,B), 
                      what_in_cell(Board, A, B, W), 
                      change_cell(Board, A, B, W, NewW)),
         ABWs),
 findall([A,B], member([A,B,b], ABWs), NewAliveBlues),
 findall([A,B], member([A,B,r], ABWs), NewAliveReds).

%%%%% what_in_cell/4

what_in_cell([AliveBlues, _], A, B, 'b') :-
 member([A,B], AliveBlues).

what_in_cell([_, AliveReds], A, B, 'r') :-
 member([A,B], AliveReds).

what_in_cell([AliveBlues, AliveReds], A, B, ' ') :-
 \+ member([A,B], AliveBlues), 
 \+ member([A,B], AliveReds).

%%%%% cchange_cell/5

change_cell([AliveBlues, AliveReds], A, B, W, NewW) :-
 findall(b, (neighbour_position(A,B,[NA,NB]),
             member([NA,NB], AliveBlues)),
         Bs),
 findall(r, (neighbour_position(A,B,[NA,NB]),
             member([NA,NB], AliveReds)),
         Rs),
 length(Bs, BL),
 length(Rs, RL),
 populate_cell(BL,RL,W,NewW),
 !.

%%%%% neighbour_position/3

neighbour_position(A,B,[I,J]) :-
 AM1 is A - 1,
 AP1 is A + 1,
 BM1 is B - 1,
 BP1 is B + 1,
 L = [AM1,A,AP1],
 K = [BM1,B,BP1],
 member(I,L),
 member(J,K),
 \+ (I == A, J == B),
 \+ I == 0,
 \+ J == 0,
 \+ I > 8,
 \+ J > 8.

%%%%% populate_cell/4

populate_cell(3,0,' ',b).

populate_cell(0,3,' ',r).

populate_cell(2,1,' ',b).

populate_cell(1,2,' ',r).

populate_cell(NumBlues,NumReds,X,X) :-
 2 is NumBlues + NumReds.

populate_cell(NumBlues,NumReds,X,X) :-
 3 is NumBlues + NumReds.

populate_cell(_,_,_,' ').

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% DRAWING THE BOARD

%%%%% draw_board/1
%
% wrapper for draw_board/2

draw_board(Board) :-
 draw_board(verbose, Board).

%%%%% draw_board/2
%
% draws a board, if needed

draw_board(quiet, _) :-
 !.

draw_board(verbose, Board) :-
 format('  12345678~n +--------+', []),
 draw_cells(1, 1, Board),
 format('~n +--------+~n~n', []).

%%%%% draw_cells/3
%
% draw the right colour in cells

% beginning of row

draw_cells(A, 1, ColouredCells) :-
 !,
 format('~n~w|', [A]),
 draw_cell(A, 1, ColouredCells, NewColouredCells),
 draw_cells(A, 2, NewColouredCells).

% end of row

draw_cells(A, 8, ColouredCells) :-
 !,
 draw_cell(A, 8, ColouredCells, NewColouredCells),
 format('|', []),
 (
  A = 8
  -> true
  ;  A1 is A + 1,
     draw_cells(A1, 1, NewColouredCells)
 ).

% middle of row

draw_cells(A, B, ColouredCells) :-
 draw_cell(A, B, ColouredCells, NewColouredCells),
 B1 is B + 1,
 draw_cells(A, B1, NewColouredCells).

%%%%% draw_cell/4
%
% draw the right colour in a cell

draw_cell(A, B, [[[A,B]|RestBlues],Reds], [RestBlues,Reds]) :-
 !,
 format('b', []).

draw_cell(A, B, [Blues,[[A,B]|RestReds]], [Blues,RestReds]) :-
 !,
 format('r', []).

draw_cell(_, _, ColouredCells, ColouredCells) :-
 format(' ', []).

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% SHOWING THE SCORE

%%%%% show_score/2

show_score(quiet, _) :-
 !.

show_score(verbose, [AliveBlues, AliveReds]) :-
 length(AliveBlues, BL),
 length(AliveReds, RL),
 format('~nblue score = ~w~nredscore = ~w~n~n', [BL,RL]).

%%%%% show_move/4

show_move(quiet, _, _, _) :-
 !.

show_move(verbose, Num, Player, Move) :-
 format('~w. ~w moves ~w~n~n', [Num,Player,Move]).

%%%%% show_winner/3

show_winner(quiet, _, _) :-
 !.

show_winner(verbose, 'Exhaust', Num) :-
 format('Game is drawn due to exhaustion after ~w moves!~n~n', [Num]).

show_winner(verbose, 'Draw', Num) :-
 format('Game is drawn after ~w moves!~n~n', [Num]).

show_winner(verbose, Winner, Num) :-
 format('~w wins after ~w moves!~n~n', [Winner,Num]).


