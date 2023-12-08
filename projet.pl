:- use_module(library(random)).
:- dynamic echo_on/0.

% Definition opérateur ?=
:- op(20,xfy,?=).

% Pour enlever les warning de Singleton variables
:- style_check(-singleton).



% QUESTION 1

% occur_check/2 vérifie si une variable V n'apparaît pas dans la structure T.
% Ce prédicat est récursif pour traiter les cas où T est un terme composé.
% Si la variable V est trouvée dans la structure T, le prédicat échoue.

occur_check(V, T) :-
    var(V),            % Vérifie si V est une variable
    compound(T),       % Vérifie si T est un terme composé
    arg(_, T, X),      % Récupère le sous-terme X de T
    (V == X;           % Si V est égal à X, le prédicat réussit
    (compound(X),      % Sinon, si X est un terme composé
    occur_check(V, X))), % Applique récursivement occur_check sur X
    !.                 % Utilise le cut pour éviter la recherche de solutions alternatives



% règles pour la question 1



regles(E, rename) :- 
    arg(1, E, X),    % Récupère le premier argument de l'équation E dans la variable X
    arg(2, E, T),    % Récupère le deuxième argument de l'équation E dans la variable T
    var(X),          % Vérifie si X est une variable
    var(T).          % Vérifie si T est une variable


regles(E, simplify) :- 
    arg(1, E, X),    % Récupère le premier argument de l'équation E dans la variable X
    arg(2, E, T),    % Récupère le deuxième argument de l'équation E dans la variable T
    atomic(T),       % Vérifie si T est atomic
    var(X).          % Vérifie si X est une variable.


regles(E, expand) :- 
    arg(1, E, X), 
    arg(2, E, T),
    var(X),
    compound(T), 
    \+occur_check(X, T).



% Défintion de la règle Check
regles(E, check) :- 
    arg(1, E, X),
    arg(2, E, T),
    \+X == T, 
    var(X), 
    compound(T), 
    occur_check(X, T), 
    !. 
    

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
    \+ (A==C, B == D),
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
    write("Systeme déquation unifiable et unifié hehe."),
    !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% QUESTION 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Les poids pour choix_pondere_1 
poids_ponderation(clash, 4, ponderation1).
poids_ponderation(check, 4, ponderation1).
poids_ponderation(rename, 3, ponderation1).
poids_ponderation(simplify, 3, ponderation1).
poids_ponderation(orient, 2, ponderation1).
poids_ponderation(decompose, 1, ponderation1).
poids_ponderation(expand, 0, ponderation1).


% Les poids pour choix_pondere_2 
poids_ponderation(clash, 4, ponderation2).
poids_ponderation(check, 4, ponderation2).
poids_ponderation(rename, 2, ponderation2).
poids_ponderation(simplify, 1, ponderation2).
poids_ponderation(orient, 3, ponderation2).
poids_ponderation(decompose, 2, ponderation2).
poids_ponderation(expand, 0, ponderation2).


% Les poids pour choix_pondere_3
poids_ponderation(clash, 4, ponderation3).
poids_ponderation(check, 4, ponderation3).
poids_ponderation(rename, 3, ponderation3).
poids_ponderation(simplify, 3, ponderation3).
poids_ponderation(orient, 2, ponderation3).
poids_ponderation(decompose, 1, ponderation3).
poids_ponderation(expand, 0, ponderation3).


% Définition de Unifie
unifie([], Strat) :- 
    write("on est good pour la stratégie "),
    write(Strat),
    writeln("."),
    !.

unifie([E|P], choix_premier) :-
    print_systeme([E|P]),
    regles(E, R),
    print_regles(R, E),
    reduit(R, E, P, Q),
    !,
    unifie(Q, choix_premier).

unifie([E|P], choix_pondere_1) :- 
    print_systeme([E|P]),
    regles(E, R),
    print_regles(R, E),
    poids_max(P, Q, E, R, [], ponderation1),
    !, 
    unifie(Q, choix_pondere_1).


unifie([E|P], choix_pondere_2) :- 
    print_systeme([E|P]),
    regles(E, R), 
    print_regles(R, E),
    poids_max(P, Q, E, R, [], ponderation2),
    !,
    unifie(Q, choix_pondere_2).


unifie([E|P], choix_pondere_3) :- 
    print_systeme([E|P]),
    regles(E, R), 
    print_regles(R, E),
    poids_max(P, Q, E, R, [], ponderation3),
    !,
    unifie(Q, choix_pondere_3).


% Prédicat unifie/2 avec choix aléatoire de règles
% Unifie le système d'équations P en choisissant aléatoirement une équation et en appliquant la règle associée.
% L'algorithme se poursuit récursivement jusqu'à ce que le système soit entièrement unifié.

unifie(P, choix_aleatoire) :-
    print_systeme(P),
    % Calcul de la taille du système d'équations
    length(P, Size), 
    % Génération d'un indice aléatoire dans la plage de la taille du système
    random(0, Size, Random), 
    % Extraction d'une équation aléatoire (E) du système, avec le reste des équations (Reste)
    nth0(Random, P, E, Reste),
    % Obtention de la règle associée à l'équation
    regles(E, R),
    print_regles(R, E),
    % Réduction du système en appliquant la règle à l'équation sélectionnée
    reduit(R, E, Reste, Q), 
    % Condition d'arrêt : le système est entièrement unifié
    !, 
    % Appel récursif avec le nouveau système Q
    unifie(Q, choix_aleatoire).



%%%%%%%%%%%%%%%%%%% Prédicats auxiliaires %%%%%%%%%%%%%%%%%%%


poids_max([], Q, E, R, L, Ponderation) :-
    reduit(R, E, L, Q), 
    !.

poids_max([F|P], Q, E, RegleE, Result, Ponderation) :-
    meilleur_poids([E, F], X, Reste, Regle, Ponderation),
    !,
    poids_max(P, Q, X, Regle, [Reste|Result], Ponderation).

% Ajout du prédicat poids/4
poids(Ponderation, E, Poids, Regle) :-
    regles(E, Regle),
    poids_ponderation(Regle, Poids, Ponderation).



meilleur_poids([E, F], X, Reste, Regle, Ponderation) :-
    poids(Ponderation, E, PoidsE, RegleE),
    poids(Ponderation, F, PoidsF, RegleF),
    (PoidsE > PoidsF -> (X = E, Reste = F, Regle = RegleE); (X = F, Reste = E, Regle = RegleF)).












    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% QUESTION 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% set_echo: ce prédicat active l'affichage par le prédicat echo
set_echo :- 
    assert(echo_on).

% clr_echo: ce prédicat inhibe l'affichage par le prédicat echo
clr_echo :- 
    retractall(echo_on).

% Predicat echo pour afficher les règles si l'écho est activé
echo(Message) :-
    ( echo_on -> write(Message); true ).


% Prédicat unif/2 pour effectuer l'unification avec ou sans affichage des règles
unif(P, S) :-
    clr_echo,   % Inhiber l'écho à la fin de l'unification
    unifie(P, S).


% Prédicat trace_unif/2 pour effectuer l'unification avec affichage des règles à chaque étape
trace_unif(P, S) :-
    set_echo,   % Activer l'écho (affichage des règles)
    unifie(P, S).


% Prédicats pour laffichage                        
print_systeme(P) :- 
    echo('system : '), 
    echo(P), 
    echo('.\n').   


print_regles(R,E) :- 
    echo(R), 
    echo(' : '), 
    echo(E), 
    echo('.\n').  




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MENU %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Menu principal
menu_principal :-
    writeln('1. Entrer un système d''équations à unifier'),
    writeln('2. Quitter'),
    read(Choix),
    writeln(Choix),
    traiter_choix(Choix).



traiter_choix(Choix) :- 
    (Choix = 1 -> 
        writeln('Entrez votre système d''équations à unifier (sous forme de liste, par exemple, [X ?= Y, Y ?= Z]):'),
        read(Systeme),
        writeln(Systeme),
        writeln('Voulez-vous afficher le systeme et les règles à chaque itération de l\'unification ?'),
        read(Affichage),
        writeln("Quelle stratégie voulez-vous utiliser ?"),
        writeln("1. Choix premier"),
        writeln("2. Choix pondere 1"),
        writeln("3. Choix pondere 2"),
        writeln("4. Choix pondere 3"),
        writeln("5. Choix alétoire"),
        read(ChoixStrategie),
        traiter_affichage(Affichage, Systeme, ChoixStrategie)
    ; 
        writeln("Au revoir !")
    ).




traiter_affichage(Affichage, Systeme, 1) :-
    (Affichage = 'y' -> 
        trace_unif(Systeme, choix_premier)
    ; 
        unif(Systeme, choix_premier)). 






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