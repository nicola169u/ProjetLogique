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
    arg(1, E, X),    % Récupère le premier argument de l'équation E dans la variable X
    arg(2, E, T),    % Récupère le deuxième argument de l'équation E dans la variable T
    var(X),          % vérifie que X est une variable
    compound(T),     % vérifie que T n'est ni variable ni atomic
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



% regles(E, clash) : Définit la règle clash pour l'équation E.
% Cette règle est activée lorsque les termes composés X et T de l'équation E ont des
% foncteurs différents ou des arités différentes.

regles(E, clash) :-
    arg(1, E, X),                 % Extraction du premier argument de l'équation E (X).
    arg(2, E, T),                 % Extraction du deuxième argument de l'équation E (T).
    compound(X), compound(T),     % Vérification que X et T sont des termes composés.
    functor(X, A, B),              % Extraction du nom du foncteur et de l'arité pour X.
    functor(T, C, D),              % Extraction du nom du foncteur et de l'arité pour T.
    not((A==C , B==D)),    % Vérification que les foncteurs ou les arités ne sont pas les mêmes.
    !.                             % Arrête la recherche de solutions pour cette règle.








% Toute la partie pour le prédicat Réduit qui va transformer le système P en systeme Q par la règle R de l equation E

reduit(check, _, _, _) :- 
    fail. 

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

% Prédicat reduit/4 qui réduit une expression (E) en utilisant l'orientation.
reduit(orient, E, P, Q) :- 
    % Extrait le premier argument (X) du terme E.
    arg(1, E, X), 
    % Extrait le deuxième argument (T) du terme E.
    arg(2, E, T),
    % Crée une nouvelle paire X?=T et l'ajoute à la liste P pour obtenir la liste Q.
    append([X?=T], P, Q),
    !.



% Prédicat reduit/4 qui réduit une expression composée (E) en utilisant la décomposition et le remplacement.
reduit(decompose, E, P, Q) :- 
    % Extrait le premier argument (X) du terme E.
    arg(1, E, X),
    % Extrait le deuxième argument (T) du terme E.
    arg(2, E, T),
    % Décompose X en son nom et ses arguments en utilisant le prédicat decomposition_args/3.
    decomposition_args(X, _, Args1), 
    % Décompose T en son nom et ses arguments en utilisant le prédicat decomposition_args/3.
    decomposition_args(T, _, Args2), 
    % Remplace les arguments de X par ceux de T en utilisant le prédicat remplace/3.
    remplace(Args1, Args2, Result),
    % Ajoute le résultat obtenu à la liste P pour obtenir la liste Q.
    append(Result, P, Q).


reduit(clash, _, _, _) :-
    fail.



% Predicat remplace/3 qui prend trois listes en entrée et remplace chaque élément
% de la première liste par son équivalent dans la deuxième liste, en construisant
% une troisième liste résultante.

remplace([], [], []). % Cas de base : si les deux listes sont vides, la liste résultante est vide.

remplace([A|Args1], [B|Args2], [A ?= B | Temp]) :-
    % Règle récursive : remplace le premier élément de la première liste (A)
    % par le premier élément de la deuxième liste (B) et ajoute cette substitution
    % à la liste résultante (Temp).
    remplace(Args1, Args2, Temp).



% Définition de my_compound_args qui permet de renvoyer le nombre darguments aussi bien de a que de a() ou de a(X).

% Prédicat decomposition_args/3 qui décompose un terme X en son nom (Name) et ses arguments (Args).
decomposition_args(X, Name, Args) :- 
    % Cas où X est atomique.
    atomic(X), 
    % Obtient le nom et l'arité du terme X.
    functor(X, Name, Arity), 
    % Vérifie que l'arité est supérieure à 0.
    Arity > 0, 
    % Extrait les arguments du terme X en appelant le prédicat extract_args/3.
    extract_args(X, Arity, Args),
    !.

% Deuxième règle de decomposition_args/3 : s'applique lorsque X est un terme composé.
decomposition_args(X, Name, Args) :- 
    % Vérifie si X est un terme composé.
    compound(X), 
    % Obtient le nom (Name) et les arguments (Args) du terme composé X.
    compound_name_arguments(X, Name, Args).

% Prédicat extract_args/3 qui extrait les arguments d'un terme.
% Règle de base : si l'arité est 0, la liste des arguments est vide.
extract_args(_, 0, []).

% Règle récursive de extract_args/3 : extrait l'argument N du terme X et continue la récursion.
extract_args(X, N, [Arg|Args]) :- 
    % Extrait l'argument N du terme X.
    arg(N, X, Arg), 
    % Décrémente N pour passer à l'argument suivant.
    N1 is N - 1, 
    % Appelle récursivement extract_args/3 avec le nouvel argument.
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


% Définition de ponderation_1 (clash, check > rename, simplify > orient > decompose > expand)
poids_ponderation(ponderation1, E, 4, clash) :- regles(E, clash).
poids_ponderation(ponderation1, E, 4, check) :- regles(E, check).
poids_ponderation(ponderation1, E, 3, rename) :- regles(E, rename).
poids_ponderation(ponderation1, E, 3, simplify) :- regles(E, simplify).
poids_ponderation(ponderation1, E, 2, orient) :- regles(E, orient).
poids_ponderation(ponderation1, E, 1, decompose) :- regles(E, decompose).
poids_ponderation(ponderation1, E, 0, expand) :- regles(E, expand).

% Définition de ponderation_2 (clash, check > orient > decompose > rename, simplify > expand)
poids_ponderation(ponderation2, E, 4, clash) :- regles(E, clash).
poids_ponderation(ponderation2, E, 4, check) :- regles(E, check).
poids_ponderation(ponderation2, E, 3, orient) :- regles(E, orient).
poids_ponderation(ponderation2, E, 2, decompose) :- regles(E, decompose).
poids_ponderation(ponderation2, E, 2, rename) :- regles(E, rename).
poids_ponderation(ponderation2, E, 1, simplify) :- rreglesegle(E, simplify).
poids_ponderation(ponderation2, E, 0, expand) :- regles(E, expand).

% Définition de ponderation_3 (clash, check > orient > decompose > expand > rename, simplify)
poids_ponderation(ponderation3, E, 4, clash) :- regles(E, clash).
poids_ponderation(ponderation3, E, 4, check) :- regles(E, check).
poids_ponderation(ponderation3, E, 3, orient) :- regles(E, orient).
poids_ponderation(ponderation3, E, 2, decompose) :- regles(E, decompose).
poids_ponderation(ponderation3, E, 1, expand) :- regles(E, expand).
poids_ponderation(ponderation3, E, 0, rename) :- regles(E, rename).
poids_ponderation(ponderation3, E, 0, simplify) :- regles(E, simplify).

% Définition de Unifie
unifie([], _) :- 
    writeln("Unifiable"),
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
    poids_max(P, Q, E, R, [], ponderation1),
    print_regles(R, E),
    !, 
    unifie(Q, choix_pondere_1).


unifie([E|P], choix_pondere_2) :- 
    print_systeme([E|P]),
    regles(E, R),
    poids_max(P, Q, E, R, [], ponderation1),
    print_regles(R, E),
    !,
    unifie(Q, choix_pondere_2).


unifie([E|P], choix_pondere_3) :- 
    print_systeme([E|P]),
    regles(E, R),
    poids_max(P, Q, E, R, [], ponderation1),
    print_regles(R, E),
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
        writeln('Voulez-vous afficher le systeme et les règles à chaque itération de l\'unification ?(y/n)'),
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
        trace_unif(Systeme, choix_premier),
        afficher_variables_finales(Systeme)
    ; 
        unif(Systeme, choix_premier),
        afficher_variables_finales(Systeme)).





%%%%%%%%%%%%%%%%%%% Prédicats auxiliaires %%%%%%%%%%%%%%%%%%%


% Prédicat poids_max/6 pour calculer le poids maximum d'un ensemble d'expressions.
poids_max([], Q, E, R, L, Ponderation) :-
    % Utilise le prédicat reduit/4 pour réduire l'expression E avec la règle R et la liste L,
    % puis assigne le résultat à la variable Q.
    reduit(R, E, L, Q),
    !.

% Règle récursive de poids_max/6 pour traiter chaque élément de la liste de poids.
poids_max([F|P], Q, E, RegleE, Result, Ponderation) :-
    % Utilise le prédicat meilleur_poids/5 pour obtenir la meilleure paire d'expressions.
    meilleur_poids([E, F], X, Reste, Regle, Ponderation),
    !,
    % Appelle récursivement poids_max/6 avec le reste de la liste de poids.
    poids_max(P, Q, X, Regle, [Reste|Result], Ponderation).

% Ajout du prédicat poids/4 pour calculer le poids d'une expression avec une pondération.
poids(Ponderation, E, Poids, Regle) :-
    % Obtient la règle associée à l'expression E.
    regles(E, Regle),
    % Calcule le poids de l'expression en utilisant la pondération spécifiée.
    poids_ponderation(Ponderation, E, Poids, Regle).

% Prédicat meilleur_poids/5 pour trouver la meilleure paire d'expressions par rapport à leur poids.
meilleur_poids([E, F], X, Reste, Regle, Ponderation) :-
    % Calcule le poids de chaque expression en utilisant la pondération spécifiée.
    poids(Ponderation, E, PoidsE, RegleE),
    poids(Ponderation, F, PoidsF, RegleF),
    % Compare les poids pour déterminer la meilleure paire d'expressions.
    (PoidsE > PoidsF -> (X = E, Reste = F, Regle = RegleE); (X = F, Reste = E, Regle = RegleF)).


% Prédicat afficher_variables_finales/1 pour afficher le résultat final.
afficher_variables_finales(Systeme) :-
    % Affiche un message indiquant que c'est le résultat final.
    writeln("Résultat final :"),
    % Appelle le prédicat auxiliaire afficher_variables_finales_rec/1 pour afficher les variables finales.
    afficher_variables_finales_rec(Systeme).

% Prédicat auxiliaire afficher_variables_finales_rec/1 pour afficher les variables finales récursivement.
afficher_variables_finales_rec([]).
afficher_variables_finales_rec([Var ?= Value|P]) :-
    % Affiche la variable et sa valeur.
    writeln(Var = Value),
    % Appelle récursivement le prédicat avec le reste de la liste.
    afficher_variables_finales_rec(P).



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