:- dynamic(grafo/2).
:- dynamic(ciudad/2).

cell_cost(Point, Cost) :-
    ciudad(Point, Char),
    char_cost(Char, Cost).

char_cost('A', 1).
char_cost('T', 1).
char_cost('.', 1).
char_cost('-', 10).
char_cost('/', 4).

:- [loading].

add(Point) :-
    [X, Y] = Point,
    X1 is X-1,
    try_add(Point, [X1, Y]),
    X2 is X+1,
    try_add(Point, [X2, Y]),
    Y1 is Y-1,
    try_add(Point, [X, Y1]),
    Y2 is Y+1,
    try_add(Point, [X, Y2]),!.

try_add(P1, P2) :-
    add_arc(P1, P2), !; true.

add_arc(P1, P2) :-
    grafo(P1, P2).
add_arc(P1, P2) :-
    not_wall(P1),
    not_wall(P2),
    assert(grafo(P1, P2)), 
    assert(grafo(P2, P1)).

not_wall(Point) :-
    ciudad(Point, C),
    \+ C = '='.

main(Filename) :-
    prepare(Filename),
    start(Start),
    end(End),
    a_star(Start, End, Path),
    f(Path, F),
    write('Лучший маршрут: ('), print(F), write(')'), print(Path), nl, nl,
    write('Последовательность действий: '), nl, print_path(Path).

start(Point) :-
    ciudad(Point, 'A').
    
end(Point) :-
    ciudad(Point, 'T').

print_path([Cur, Nxt | Rest]) :-
    dir_word(Cur, Nxt, Word), !,
    write(Word), nl,
    print_path([Nxt | Rest]).

dir_word(_, Fence, 'залезть на забор') :-
    ciudad(Fence, '-').
dir_word(_, Door, 'открыть дверь') :-
    ciudad(Door, '/').
dir_word([X1, Y1], [X2, Y1], 'шаг вправо') :-
    succ(X1, X2).
dir_word([X1, Y1], [X2, Y1], 'шаг влево') :-
    succ(X2, X1).
dir_word([X1, Y1], [X1, Y2], 'шаг вниз') :-
    succ(Y1, Y2).
dir_word([X1, Y1], [X1, Y2], 'шаг вверх') :-
    succ(Y2, Y1).

prepare(Filename) :-
    read_file(Filename),
    load_grafo.
    
load_grafo :-
    foreach(ciudad(Point, _), add(Point)).

print_grafo :-
    bagof(P1-P2, grafo(P1, P2), Arcs), !,
    foreach(member(Arc, Arcs), print(Arc)).

a_star(Start, Goal, Path) :-
    once(search([[Start]], Goal, Path)).

search(_, _, BestPath) :-
    \+ BestPath = [], !.
search(Open, Goal, BestPath) :-
    sort_by_priority(Open, Sorted, Goal),
    Sorted = [Path | RestOpen],
    last(Path, Cur),
    check_result(Cur, Goal, Path, BestPath),
    bagof(Neib, grafo(Cur, Neib), Bag),
    include(not_visited(Path), Bag, Filtered),
    maplist(append_to_path(Path), Filtered, NewPaths),
    append(NewPaths, RestOpen, NewOpen),
    search(NewOpen, Goal, BestPath).

sort_by_priority(Open, Paths, Goal) :-
    map_list_to_pairs(priority(Goal), Open, Pairs),
    keysort(Pairs, Sorted),
    pairs_values(Sorted, Paths).

priority(Goal, Path, P) :-
    f(Path, F),
    last(Path, Cur),
    h(Goal, Cur, H),
    P is 0.1*F + H.

f(Path, F) :-
    maplist(cell_cost, Path, Costs), !,
    sum_list(Costs, F).

h([X1, Y1], [X2, Y2], H) :-
    H is sqrt((X2-X1)^2 + (Y2-Y1)^2).

append_to_path(Path, Cell, NewPath) :-
    append(Path, [Cell], NewPath).

not_visited(Path, Cell) :-
    \+ member(Cell, Path).

check_result(Goal, Goal, Path, Path).
check_result(_, _, _, _).