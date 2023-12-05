% Definition opérateur ?=
:- op(20,xfy,?=).

% Pour enlever les warning de Singleton variables
:- style_check(-singleton).

% QUESTION 1

% règles pour la question 1



regles(E, rename) :- 
    arg(1, E, X),
    arg(2, E, T),
    var(X),
    var(T), 
    X = T,
    !.


regles(E, simplify) :- 
    arg(1, E, X),
    arg(2, E, T),
    atomic(T),
    var(X), 
    X = T,
    !.


regles(E, expand) :- 
    arg(1, E, X), 
    arg(2, E, T),
    var(X),
    compound(T), 
    occur_check(X, T),
    !.

regles(E, decompose) :- 
    arg(1, E, X), 
    arg(2, E, T),
    compound(X), % on vérifie que X nest ni une variable ou ni une constante
    compound(T), % on vérifie que T nest ni une variable ou ni une constante
    functor(X, A, B), % ici on récupère le nom de la fonction dans A et le nombre de paramètre dans B pour X
    functor(T, C, D), % ici on récupère le nom de la fonction dans A et le nombre de paramètre dans B pour T
    A == C, % on test si cest les memes noms de fonctions
    B == D, % on test si on a bien le meme nombre de paramètres
    !.


regles(E, orient) :- 
    arg(1, E, X), 
    arg(2, E, T),
    var(X),
    nonvar(T),
    !.


regles(E, clash) :- 
    arg(1, E, X), 
    arg(2, E, T),
    compound(X), 
    compound(T),
    (functor(X, A, B) = functor(T, C, D)),
    !.


regles(E, check) :- 
    arg(1, E, X),
    arg(2, E, T),
    (var(X), var(T) -> 
        X == T ;
    compound(T) -> 
        member(X, T)),
    !.

occur_check_var(X, T) :- 
    X == T,
    !.

occur_check_list(X, T) :- 
    member(X, T),
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