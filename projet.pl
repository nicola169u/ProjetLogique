% Definition opérateur ?=
:- op(20,xfy,?=).

% QUESTION 1

% prédicat occur_check


occur_check(V, T) :-
    var(T),       % Vérifie si T est une variable
    V == T.       % Vérifie si V est égal à T

occur_check(V, T) :-
    nonvar(T),           % Vérifie si T nest pas une variable
    T =.. [_|Args],      % Démonte T en une liste darguments
    member(Arg, Args),   % Sélectionne chaque argument dans la liste
    occur_check(V, Arg). % Appelle récursivement occur_check pour chaque argument



