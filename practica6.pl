% PRACTICA 6
% DAVID SEIJAS PEREZ

% Ejercicio 1
% Usando igualdad sintáctica:

elimina1([ ], X, [ ]).
elimina1([X|R], Y, NR) :- Y == X, elimina1(R, Y, NR).
elimina1([X|R], Y, [X|NR]) :- Y \== X, elimina1(R, Y, NR).

% Usando unificación:

elimina2([ ], X, [ ]).
elimina2([X|R], Y, NR) :- Y = X, elimina2(R, Y, NR).
elimina2([X|R], Y, [X|NR]) :- Y \= X, elimina2(R, Y, NR).

% Combinando las dos anteriores:

elimina3([ ], X, [ ]).
elimina3([X|R], X, NR) :- elimina3(R,X,NR).
elimina3([X|R], Y, [X|NR]) :- Y \== X, elimina3(R,Y,NR).



% elimina1([a, b, a, c], a, L). 
% L = [b, c]
% elimina1([a, b, a, c], X, L). 
% L = [a, b, a, c];
% No elimina nada pues al hacer igualdad sintáctica la X no va a coincidir con ninguna letra

% elimina2([a, b, a, c], a, L).
% L = [b, c] 
% elimina2([a, b, a, c], X, L).
% X = a,
% L = [b, c] ;

% elimina3([a, b, a, c], a, L). 
% L = [b, c]
% elimina3([a, b, a, c], X, L).
% X = a,
% L = [b, c] ;
% X = b,
% L = [a, a, c] ;
% X = a,
% L = [a, b, c] ;
% X = c,
% L = [a, b, a] ;
% L = [a, b, a, c] ;
% X = a ;


% Para la primera llamada todas las funciones se comportan igual pues el argumento que se pasa como Y es a (un elemento de la lista)
% Por tanto, la a va unificar con la a de la lista y va a dar cierto al hacer la igualdad sintáctica.
% elimina1 solo dará como solución la lista entera pues la variable X no será sintácticamente igual a ningún elemento de la lista.
% elimina2 solo saca como solución la lista en la que elimina a la a pues X unifica con la a y elimina los elementos de la lista que son i
% unifican con a, pero no unificará con ningún elemento más
% elimina3 mostrará todas las posibles soluciones pues puede entrar en una definición u otra



% Ejercicio 2

arbol_binario(void).
arbol_binario(arbol(_, I, D)) :- arbol_binario(I), arbol_binario(D).

sumatree(void, 0).
sumatree(arbol_binario(Y, I, D), X) :- integer(Y), sumatree(I, X1),sumatree(D, X2), X is Y+X1+X2.

max(X1, X2, X3, X) :- Y is max(X1, X2), X is max(Y, X3).

maximo(void, 0).
maximo(arbol_binario(Y, I, D), X) :- integer(Y), Y @> 0, maximo(I, X1), maximo(D, X2), max(Y, X1, X2, X).



% Ejercicio 3

prefijo([], _).
prefijo([X|Xs], [X|Ys]) :- prefijo(Xs, Ys).

sufijo(Xs, Xs).
sufijo(Xs, [_|Ys]) :- sufijo(Xs, Ys).

sublista(Xs, Ys) :- prefijo(Xs, Ys).
sublista(Xs, [_|Ys]) :- sublista(Xs, Ys).

sublistas(Xs, Xss) :- setof(S, sublista(S, Xs), Xss).



% Ejercicio 4

hanoi(0, , , , ).
hanoi(1, A, B, C, [A,B]).
hanoi(N, A, B, C, M) :- N>1, N1 is N-1, hanoi(N1,A,C,B,M1), append(M1,[A,B|M2],M), hanoi(N1,C,B,A,M2).


