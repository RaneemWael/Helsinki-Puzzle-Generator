grid_build(N, M):-
	grid_build2(N, M, N).
	
grid_build2(_, [], 0).

grid_build2(X, [H|T], Y):-
	Y>0,
	length(H, X),
	N1 is Y-1,
	grid_build2(X, T, N1).

	

grid_gen(N, M):-
	grid_build(N, M),
	grid_gen2(N, M, N).
	
grid_gen2(_, [], 0).

grid_gen2(N, [H|T], Y):-
	Y>0,
	check_range(H, N),
	Y1 is Y-1,
	grid_gen2(N, T, Y1).
	
check_range([], _).

check_range([H|T], N):-
	between(1, N, H),
	check_range(T, N).


num_gen(F, L, R):-
	length(R, L),
	num_gen2(F, L , R).
	
num_gen2(_, _, []).

num_gen2(F, L ,[F|T]):-
	F1 is F+1,
	num_gen2(F1, L, T).	

	
check_num_grid(G):-
	flatten(G, L),
	sort(L, R),
	max_list(R, M),
	length(G, Len),
	M=<Len,
	num_gen(1, M, R).
	
	

acceptable_permutation(L, R):-
	acceptable_permutation2(L, R),
	check(R, L).

acceptable_permutation2([],[]).
acceptable_permutation2([H|T], R):- 
	acceptable_permutation2(T,P), 
	insert(H,P,R).
	
insert(X, L, [X|L]).
insert(X, [H|T], [H|T1]):- 
	insert(X, T, T1).


check([], []).
check([H1|T1], [H2|T2]):-
	H1\=H2,
	check(T1, T2).


acceptable_distribution(G):-
	acceptable_distribution(G, G, 1).
	
acceptable_distribution(_, [], _).

acceptable_distribution(G, [H|T], X):-
	get_col(G, X, R),
	H\=R,
	X1 is X+1,
	acceptable_distribution(G, T, X1).
	
get_col([], _, []).
get_col([H|T], X, R):-
	get_col(T, X, R1),
	loop(H, 1, X, E),
	R = [E|R1].

loop([H|_], Y, X, H):- Y=X.
loop([_|T], Y, X, E):-
	X>Y,
	Y1 is Y+1,
	loop(T, Y1, X, E).



row_col_match(G):-
	acceptable_distribution(G),
	row_eq_col(G, G, 1).

row_eq_col(_, [], _).
row_eq_col(G, [H|T], N):-
	list_eq_col(G, H, 1),
	row_eq_col(G, T, N).

list_eq_col(G, L, N):- get_col(G, N, R), R=L.
list_eq_col(G, L, N):-
	get_col(G, N, R),
	R\=L,
	N1 is N+1,
	list_eq_col(G, L, N1).



trans(M, M1):-
	length(M, X),
	trans(M, M1, 1, X).

trans(_, [], _, 0).

trans(M, [H|T], X, Y):-
	get_col(M, X, R),
	H = R,
	X1 is X+1,
	Y1 is Y-1,
	trans(M, T, X1, Y1).
	
	
	
distinct_rows([]).	
distinct_rows([H|T]):-
	distinct_rows2([H|T]),
	distinct_rows(T).
	
distinct_rows2([_]).
distinct_rows2([H1, H2|T]):-
	H1\=H2,
	distinct_rows([H1|T]).
	
	
distinct_columns(M):-
	trans(M, M1),
	distinct_rows(M1).
	
	
	
helsinki(N, G):-
	grid_gen(N, G),
	check_num_grid(G),
	row_col_match(G),
	distinct_rows(G),
	distinct_columns(G).
	
	
	