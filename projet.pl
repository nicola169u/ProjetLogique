% Definition opérateur ?=
:- op(20,xfy,?=).

% Pour enlever les warning de Singleton variables
:- style_check(-singleton).

echo(T) :- echo_on, !, write(T).

% QUESTION 1

occur_check(V, T) :-
    var(V),
    compound(T), 
    arg(_, T, X), 
    (V == X;
    (compound(X), 
    occur_check(V,X))),
    !.


% règles pour la question 1



regles(E, rename) :- 
    arg(1, E, X),
    arg(2, E, T),
    var(X),
    var(T).


regles(E, simplify) :- 
    arg(1, E, X),
    arg(2, E, T),
    atomic(T),
    var(X).


regles(E, expand) :- 
    arg(1, E, X), 
    arg(2, E, T),
    var(X),
    compound(T), 
    \+occur_check(X, T).

regles(E, orient) :- 
    arg(1, E, X), 
    arg(2, E, T),
    var(X),
    nonvar(T).

regles(E, decompose) :- 
    arg(1, E, X), 
    arg(2, E, T),
    compound(X), % on vérifie que X nest ni une variable ou ni une constante
    compound(T), % on vérifie que T nest ni une variable ou ni une constante
    functor(X, A, B), % ici on récupère le nom de la fonction dans A et le nombre de paramètre dans B pour X
    functor(T, C, D), % ici on récupère le nom de la fonction dans A et le nombre de paramètre dans B pour T
    A == C, % on test si cest les memes noms de fonctions
    B == D. % on test si on a bien le meme nombre de paramètres



regles(E, clash) :- 
    arg(1, E, X), 
    arg(2, E, T),
    compound(X), 
    compound(T),
    functor(X, A, B),
    functor(T, C, D),
    not(A==C, B == D),
    !.
    




% Toute la partie pour le prédicat Réduit qui va transformer le système P en systeme Q par la règle R de l equation E

reduit(check, _, _, _) :- !, fail.

reduit(simplify, E, P, Q) :- 
    arg(1, E, X),
    arg(2, E, T),
    X = T,
    Q = P.


reduit(rename, E, P, Q) :- 
    arg(1, E, X), 
    arg(2, E, T),
    X = T, 
    Q = P.


reduit(expand, E, P, Q) :- 
    arg(1, E, X), 
    arg(2, E, T),
    X = T, 
    Q = P.

reduit(orient, E, P, Q) :- 
    arg(1, E, X), 
    arg(2, E, T),
    append([X?=T], P , Q),
    !.


% Définition de réduction de Decompose
reduit(decompose, E, P, Q) :- 
    arg(1, E, X),
    arg(2, E, T),
    decomposition_args(X, _, Args1), 
    decomposition_args(T, _, Args2), 
    remplace(Args1, Args2, Result),
    append(Result, P, Q).

reduit(clash, _, _, _) :-
    !, 
    fail.



remplace([], [], []).
remplace([A|Args1], [B|Args2], [A ?= B | Temp]) :- remplace(Args1, Args2, Temp).


% Définition de my_compound_args qui permet de renvoyer le nombre darguments aussi bien de a que de a() ou de a(X).

decomposition_args(X, Name, Args) :- 
    atomic(X), 
    functor(X, Name, Arity), 
    Arity > 0, 
    extract_args(X, Arity, Args),
    !.

decomposition_args(X, Name, Args) :- 
    compound(X), 
    compound_name_arguments(X, Name, Args).

extract_args(_, 0, []).
extract_args(X, N, [Arg|Args]) :- 
    arg(N, X, Arg), 
    N1 is N - 1, 
    extract_args(X, N1, Args).




unifie([E|P]) :-
    write("System :"),
    writeln([E|P]),
    regles(E, R), 
    reduit(R, E, P, Q),
    !, 
    unifie(Q).

unifie([]) :-
    write("Systeme d'équation unifiable et unifié hehe."),
    !.



    
    









%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test1 :- 
    E = ?=(Var1, Var2),  
    regles(E, rename),   
    write('Test 1 réussi!'), nl.

test2 :- 
    E = ?=(Var1, 42),    
    \+ regles(E, rename), 
    write('Test 2 réussi!'), nl.

test3 :- 
    E = ?=(X, Y),        
    regles(E, rename),  
    write('Test 3 réussi!'), nl.

% Exécuter les tests
run_tests :-
    test1,
    test2,
    test3.