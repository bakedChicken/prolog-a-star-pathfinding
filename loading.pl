% :- module(loading, [read_file/0]).
read_file(Name) :-
    open(Name, read, Str),
    set_input(Str),
    read_chars,
    close(Str).

read_chars :-
    get_char(StartChar),
    handle_char(0, 0, StartChar).

handle_char(_, _, end_of_file).
handle_char(X, Y, Char) :-
    store_cell(X, Y, Char),
    updateX(Char, X, NewX),
    updateY(Char, Y, NewY),
    get_char(NewChar),
    handle_char(NewX, NewY, NewChar), !.

store_cell(_, _, '\n').

store_cell(X, Y, Char) :-
    assertz(ciudad([X, Y], Char)).

updateX('\n', _, 0).

updateX(_, OldX, NewX) :-
    NewX is OldX+1.

updateY('\n', OldY, NewY) :-
    NewY is OldY+1.

updateY(_, OldY, OldY).
