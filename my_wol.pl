:- use_module(library(system)).
% - - - - -  QUESTION 3 - - - - - 
test_strategy(N, St_1, St_2):-
  test_strategy_h(N, St_1, St_2, N, 0, 0, 0, 0, 250, 0, 0),!.

test_strategy_h(N, _, _, 0, Draws, BlueWins, RedWins, Longest, Shortest, TimeSum, MoveSum):-
  format('Blue Wins:~t~w~n', [BlueWins]),
  format('Red Wins:~t~w~n', [RedWins]),
  format('Draws:~t~w~n', [Draws]),
  format('Longest Game:~t~w~n', [Longest]),
  format('Shortest Game:~t~w~n', [Shortest]),
  AvgTime is TimeSum / N,
  AvgMoves is MoveSum / N,
  format('Average No. Moves:~t~w~n', [AvgMoves]),
  format('Average Time:~t~w~n', [AvgTime]).  

test_strategy_h(N, St_1, St_2, K, Draws, BlueWins, RedWins, Longest, Shortest, TimeSum, MoveSum):-
  now(Start),
  play(quiet, St_1, St_2, M, W),
  now(End),
  NTimeSum is TimeSum + (End - Start),
  NK is K - 1,
  NMoveSum is MoveSum + M,
  ( M < Shortest
  ->
    NShortest is M
  ;
    NShortest is Shortest
  ),
  ( M > Longest,
    M =\= 250
  ->
    NLongest is M
  ;
    NLongest is Longest
  ),
  ( W = 'r'
  -> 
    NewRedWins is RedWins + 1, 
    test_strategy_h(N, St_1, St_2, NK, Draws, BlueWins, NewRedWins, NLongest, NShortest, NTimeSum, NMoveSum)
  ;
    W = 'b'
  ->
    NewBlueWins is BlueWins + 1, 
    test_strategy_h(N, St_1, St_2, NK, Draws, NewBlueWins, RedWins, NLongest, NShortest, NTimeSum, NMoveSum)
  ;
    NDws is Draws + 1, 
    test_strategy_h(N, St_1, St_2, NK, NDws, BlueWins, RedWins, NLongest, NShortest, NTimeSum, NMoveSum)
   ).


% ------------- QUESTION 4 ---------------------------

%_-_-_-_-_-_-_-_-_-_ BLOOD LUST _-_-_-_-_-_-_-_-_-_-_-_

bloodlust(Col, CurState, NewState, Move):-
  get_moves(Col, CurState, MovesAndStates),
  best_bloodlust(Col, MovesAndStates, 64, [], [], Move, NewState).

best_bloodlust(_, [], _, Move, State, Move, State).
best_bloodlust(Col, [[C_M, C_St]|Xs], Sc, A_M, A_St, Move, State):-
  next_generation(C_St, N_St),
  opposite_colour(Col, OpCol),
	num_pieces(OpCol, N_St, N_Sc),	  
  (
    N_Sc < Sc
   ->
    best_bloodlust(Col, Xs, N_Sc, C_M, C_St, Move, State)
  ;
    best_bloodlust(Col, Xs, Sc, A_M, A_St, Move, State)
  ).
    

%_-_-_-_-_-_-_-_-_-_- SELF PRESERVATION _-_-_-_-_-_-_-_-_-

self_preservation(Col, CurState, NewState, Move):-
  get_moves(Col, CurState, MovesAndStates),
  best_self_preservation(Col, MovesAndStates, -1, [], [], Move, NewState). 

best_self_preservation(_, [], _, Move, State, Move, State).
best_self_preservation(Col, [[CurMove, CurState]|Xs], CurMax, BestMove, BestState, Move, State):-
  next_generation(CurState, NextState),
  num_pieces(Col, NextState, NumPieces),
  (
    NumPieces > CurMax
  ->
    best_self_preservation(Col, Xs, NumPieces, CurMove, CurState, Move, State)
  ;
    best_self_preservation(Col, Xs, CurMax, BestMove, BestState, Move, State)
  ).
  

%_-_-_-_-_-_-_-_-_-_- LAND GRAB -_-_-_-_-_-_-_-_-_-_-_-_-
land_grab(Col, CurState, NewState, Move):-
   get_moves(Col, CurState, MovesAndStates),
   best_land_grab(Col, MovesAndStates, -64, [], [], Move, NewState).

best_land_grab(_, [], _, Move, State, Move, State).
best_land_grab(Col, [[CurMove, CurState]|Xs], Max, BestMove, BestState, Move, State):-
  land_grab_score(Col, CurState, Diff),
  (
    Diff > Max
  ->
    best_land_grab(Col, Xs, Diff, CurMove, CurState, Move, State)
  ;
    best_land_grab(Col, Xs, Max, BestMove, BestState, Move, State)
  ).

land_grab_score(Col, CurState, Score):-
  next_generation(CurState, NextState),
  opposite_colour(Col, OpCol),
  num_pieces(Col, NextState, A),
  num_pieces(OpCol, NextState, B),
  Score is A - B.



%_-_-_-_-_-_-_-_-_-_- MINIMAX -_-_-_-_-_-_-_-_-_-_-_-_-_-_
minimax(Col, CurState, NewState, Move):-
   get_moves(Col, CurState, MovesAndStates),
   best_minimax(Col, MovesAndStates, -64, [], [], Move, NewState).

best_minimax(_, [], _, Move, State, Move, State).
best_minimax(Col, [[ThisMove, ThisState]|Xs], MaxSoFar, BestMove, BestState, Move, State):-
  min_score(Col, ThisState, 2, ThisScore, MaxSoFar), 
  (
    ThisScore > MaxSoFar
  ->
    best_minimax(Col, Xs, ThisScore, ThisMove, ThisState, Move, State)
  ;
    best_minimax(Col, Xs, MaxSoFar, BestMove, BestState, Move, State)
  ).

% Returns the worst possible outcome from the given state. Generates all
% the possible moves and calls best_min to do the leg work.
min_score(Col, State, 1, Score, _):-
  land_grab_score(Col, State, Score),!.
min_score(Col, State, Depth, Score, MaxToBeat):-
  next_generation(State, NextState),
  opposite_colour(Col, OpCol),
  get_moves(OpCol, NextState, NewStates),
  NDepth is Depth - 1,
  best_min(Col, NewStates, NDepth, 64, Score, MaxToBeat).

% Returns the worst score possible from the list of given states.
% If at any point the Min is less than the best score from another
% move the search ends and returns the current Min (which we know will 
% not be picked). (Min < MaxToBeat).
best_min(_, [], _, Min, Min, _).
best_min(Col,[[_,State]|Xs], Depth, Min, Score, MaxToBeat):-
  max_score(Col, State, Depth, ThisScore, Min),
  (
    Min =< MaxToBeat
  ->
    best_min(Col, [], Depth, Min, Score, MaxToBeat)
  ;
    ThisScore < Min
  ->
    best_min(Col, Xs, Depth, ThisScore, Score, MaxToBeat)
  ;
    best_min(Col, Xs, Depth, Min, Score, MaxToBeat)
  ).


% Returns the maximum score for Col from State,
% assuming opposition always make worst move.
max_score(Col, State, 1, Score, _):-
  land_grab_score(Col, State, Score),!.
max_score(Col, State, Depth, Score, MinToBeat):-
  next_generation(State, NextState),
  opposite_colour(Col, OpCol),
  get_moves(OpCol, NextState, NewStates),
  NDepth is Depth - 1,
  best_max(Col, NewStates, NDepth, -64, Score, MinToBeat). 


% Returns the best score possible from the list of given states.
% If we have already processed an oppoent move that gives us a 
% worse score (MinToBeat), we should just return
eest_max(_, [], _, Max, Max, _).
best_max(Col, [[_,State]|Xs], Depth, Max, Score, MinToBeat):- 
  min_score(Col, State, Depth, ThisScore, Max),
  (
    Max >= MinToBeat
  ->
    best_max(Col, [], Depth, Max, Score, BestMin)
  ;
    ThisScore > Max
  ->
    best_max(Col, Xs, Depth, ThisScore, Score, BestMin)
  ;
    best_max(Col, Xs, Depth, Max, Score, BestMin)
  ).

%------------------ Helper Methods ---------------------------

% Returns a list of all the legal moves red can take and the
% resulting state. 
get_moves('r', [Blues, Reds], Result):-
   findall([[A,B,MA,MB],[Blues, NewReds]],
                (
                  member([A,B], Reds),
                  neighbour_position(A,B,[MA,MB]),
		              \+member([MA,MB],Reds),
		              \+member([MA,MB],Blues),
		              alter_board([A,B,MA,MB], Reds, NewReds)
                ),
		        Result
          ).

% Returns a list of all the legal moves blue can take and the
% resulting state.
get_moves('b', [Blues, Reds], Result):-
   findall([[A,B,MA,MB],[NewBlues, Reds]],
                (
                  member([A,B], Blues),
                  neighbour_position(A,B,[MA,MB]),
		              \+member([MA,MB],Blues),
		              \+member([MA,MB],Reds),
		              alter_board([A,B,MA,MB], Blues, NewBlues)
                ),
		        Result
          ).


% Returns the number of alive blues/reds in a given state. 
num_pieces('b', [Blues, _], N):-
  length(Blues, N).
num_pieces('r', [_, Reds], N):-
  length(Reds, N).

% Returns the opponents colour.
opposite_colour('r', 'b').
opposite_colour('b', 'r').









