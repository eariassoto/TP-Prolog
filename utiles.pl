% ------------------------------------------------------------------------------
%
% UCR - Facultad de Ingeniería - ECCI
% CI-1441 Paradigmas Computacionales
% II-2015, Prof. Dr. Alvaro de la Ossa
%
% utiles.pl -- definiciones de ejemplos de predicado Prolog:
%
% a. De manejo/procesamiento de listas:
%
%    tamano/2(+L,-T): devuelve en T la cantidad de elementos de la lista L
%    nesimo/3(+N,+L,-O): devuelve en O el N-ésimo elemento de la lista L
%    miembro/2(+X,+L): devuelve 'true' si X es elemento de la lista L
%    maximo/2(+L,-M): devuelve en M el máximo elemento en la lista L
%    sumaLista/2(+L,-S): devuelve en S la suma de los elementos de la lista L
%    sumatoria/2(+L,-S): devuelve en S la expresión simbólica que representa
%    concatena/3(+L1,+L2,-C): la lista C es la concatenación de L1 y L2;
%       definición interativa (recursiva)
%    concatena2/3(+L1,+L2,-C): la lista C es la concatenación de L1 y L2;
%       definición más eficiente, basada en la unificación
%    reemplaza/4(+X,+Y,+L1,-L2): devuelve la lista L2 que resulta de
%       reemplazar en la lista L1 todas las ocurrencias de X por Y
%    reemplaza2/4(+X,+Y,+L1,-L2): versión simplificada de reemplaza/4 en la
%       que no se verifica que el objeto a reemplazar no sea una variable
%    reversa/2(+L,-R): R es la lista reversa de la lista L
%    rev/3(+L,?P,-R): auxiliar de reversa/2 que usa una pila P para invertir
%       la lista L
%
% b. De soluciones recursivas a problemas específicos (y sus auxiliares):
%
%    hanoi/4(+N,+Inicial,+Final,+Intermedia): resuelve las Torres de Hanoi
%    decir/2(+De,+A): auxiliar de hanoi/4 que muestra un movimiento
%    hanoi/1(+N): versión que usa sus propios nombres de torre; interfaz a
%       el predicado mover que es el que resuelve el juego
%    mover/4(+N,+De,+A,+Temp): resuelve las Torres de Hanoi
%    ancestro/2(+P1,+P2): determina si P2 es ancestro de P1
%              (+P1,-P2): encuentra los ancestros P2 de P1
%              (-P1,+P2): encuentra de quiénes es P2 ancestro
%              (-P1,-P2): muestra las relaciones de ancestro que se puedan
%                         encontrar en el programa Prolog
%
% c. De modificación de cláusulas del programa (adición y eliminación):
%    afirmaTodo/1(+L): afirma todas cláusulas contenidas en la lista L
%    retractaTodo/1(+P): retracta todas las cláusulas que unifiquen con el
%       patrón P; predicado de interfaz a retractalos/2, que hace el trabajo
%    retractalos/2: auxiliar de retractaTodo para retractar todas las cláusulas
%       que unifiquen con el patrón
%
% e. Otros:
%    negacion/2(+E,-V): devuelve en V el valor negado de E
%    max2/3(+X1,+X2,-M): devuelve en M el máximo entre X1 y X2
%    convierteTemp/2
%
% ------------------------------------------------------------------------------


% --- tamano/2(+L,-T) ----------------------------------------------------------
%     T es el tamaño de la lista L
%
% --- ejemplos: tamano([a,b,c],N).  -> N = 3

tamano([], 0).			% si no hay elementos, el tamaño es 0
tamano([_|R], N) :-		% en otro caso, calculamos el tamaño de la cola,
   tamano(R, M), N is M + 1.	% y el de la lista es uno más que ese


% --- nesimo/3(+N,+L,-X) -------------------------------------------------------
%     X es el N-ésimo elemento de la lista L
%
% --- ejemplo: nesimo(3,[a,b,c,d,e],X).  -> X = c

nesimo(1,[X|_],X).			% si se busca el primero, devuelve la cabeza;
nesimo(N,[_|R],E) :-			% en otro caso,reduce el contador en 1
   M is N - 1, nesimo(M,R,E).	% y recurre sobre la cola

% --- miembro/2(+X,+L)  --------------------------------------------------------
%     ¿es X miembro de L?
%
% --- ejemplo: miembro(c,[a,b,c,d,e]).  -> verdadero
miembro(X, [X|_]).			% si X es el elementos de la cabeza,
miembro(X, [_|R]) :-			% es miembro; en otro caso,
   miembro(X, R).			% lo buscamos en la cola


% --- max2/3(+X,+Y,-M) ---------------------------------------------------------
%     M es el máximo de X y Y
%
% --- nota: observe el uso del operador disyuntivo ';', en lugar de varias
%           cláusulas.

max2(X,Y,Max) :-
   X = Y, Max = Y;			% si son iguales, el máximo es cualquiera,
   X > Y, Max = X;			% si X es mayor, X es el máximo,
   Y > X, Max = Y.			% en otro caso, Y es el máximo


% --- maximo/2(+L,-M) ----------------------------------------------------------
%     M es el elemento máximo de la lista L

maximo([X],X).				% si solo hay un elemento, es el máximo
maximo([X|Xr],M) :-			% en otro caso,
   maximo(Xr,Y),			% el máximo debe ser el mayor entre
   ((X >= Y, M = X); M = Y), !.	% la cabeza y el máximo de la cola

% --- convierteTemp/2(?C,?F) ----------------------------------------------------------
%     C es la temperatura en Celsius, F en Fahrenheit
%     convierteTemp(+C,+F): determina si C y F sin equivalentes
%     convierteTemp(+C,-F): convierte de C a F
%     convierteTemp(-C,+F): convierte de F a C

convierteTemp(C,F) :-
   var(C), var(F), write('Error en los argumentos'), nl, !, fail.

convierteTemp(C,F) :-
   var(C), C is (F - 32) * 5 / 9.

convierteTemp(C,F) :-
   var(F), F is (C * 9 / 5) + 32.

convierteTemp2(C,F) :-
   var(C), C is (F - 32) * 5 / 9 ;
   var(F), F is (C * 9 / 5) + 32.

% --- sumaLista/2(+L,-S) -------------------------------------------------------
%     S es la suma de los elementos de la lista L

sumaLista([],0).			% si la lista está vacía, su suma es 0;
sumaLista([X|R],T) :-		% en otro caso, la suma de la lista es la suma
   sumaLista(R,T1), T is T1 + X.	% de la cola más el primer elemento


% --- sumatoria/2(+L,-S) ------------------------------------------------------
%     S es la sumatoria (la expresión simbólica) de los elementos de la lista L

sumatoria([],0).			% si la lista está vacía, su suma es 0;
sumatoria([X|R],T) :-		% en otro caso,añadimos a la sumatoria el
   sumatoria(R,T1), T = T1 + X.	% nuevo término


% --- concatena/3(+L1,+L2,-L3) – versión iterativa (recursiva) -----------------
%     L3 es la lista que resulta de concatenar L1 y L2

concatena([],L,L).			% concatenar una lista vacía y L es L;
concatena([X|Xr],Y,[X|Zr]) :-	% si no es vacía, la cabeza de concatenación
   concatena(Xr,Y,Zr).		% es la de la 1a lista y su cola es la concate-
						% nación de la cola de la primera y la segunda


% --- concatena2/3(+L1,+L2,-L3) – versión eficiente basada en la unificación ---
%     L3 es la lista que resulta de concatenar L1 y L2
%
% Se representa cada lista como una diferencia de dos listas: L = L1-L2.
% El truco es añadir y luego quitar a la lista un resto inexistente: la lista
% lista [a,b,c] se representa como [a,b,c|R]-R.
%
% Utilizando esta representación, la concatenación de dos listas X y Y debe
% ser X1-Y2, donde X=X1-X2 y Y=Y1-Y2.
%
% Ejemplo:
% A=[a,b,c]     se representa como A=[a,b,c|Ar]-Ar
% B=[d,e]       se representa como B=[d,e|Br]-Br
% C=[a,b,c,d,e] se representa como C=[a,b,c,d,e|Cr]-Cr
% Nótese que Cr resulta ser Br, si se concatena A-B con B-C, que debe ser A-C:

concatena2(X-Y,Y-Z,X-Z).	% la lista [a,b,..] se escribe como [a,b,..|R]-R

% Veamos cómo la concatenación de [a,b,c|Ar]-Ar con [d,e|Br]-Br] resulta ser
% la lista [a,b,c,d,e|Cr]-Cr:
%
% A = X-Y = [a,b,c|Ar] - Ar   --->  X = [a,b,c|Ar], Y = Ar
%                             --->  X = [a,b,c|Y]
% B = Y-Z = [d,e|Br]   - Br   --->  Y = [d,e|Br]  , Z = Br
%                             --->  Y = [d,e|Z]
%
% por lo tanto:
%
% C = X-Z = [a,b,c|Y]       - Z
%         = [a,b,c|[d,e|Z]] - Z
%         = [a,b,c,d,e|Z]   - Z  que representa la lista [a,b,c,d,e]


% --- reemplaza/4(+Viejo,+L1,+Nuevo,-L2) ---------------------------------------
%     L2 es la lista que resulta de reemplazar todas
%     las instancias de Viejo con Nuevo en la lista L1

reemplaza(_,[],_,[]).			% lista vacía: no hay nada que hacer
reemplaza(V,[X|R1],N,[N|R2]) :- 	% si el objeto a reemplazar es la cabeza
   nonvar(X), X = V, !,			% se reemplaza
   reemplaza(V, R1, N, R2).		% y se recorre el resto de la lista
reemplaza(V,[X|R1],N,[X|R2]) :-		% en otro caso, se deja intacta,
   reemplaza(V,R1,N,R2).			% y se recorre el resto de la lista


% --- reemplaza2/4(+Viejo,+L1,+Nuevo,+L2) --------------------------------------
%     segunda versión de reemplaza

reemplaza2(_,[],_,[]).			% lista vacía: no hay nada que hacer
reemplaza2(V,[V|R1],N,[N|R2]) :-	% si el objeto a reemplazar es la cabeza
   reemplaza2(V,R1,N,R2), !.			% se reemplaza y se proceso el resto
reemplaza2(V,[Y|R1],N,[Y|R2]) :-	% en otro caso,
   reemplaza2(V,R1,N,R2), !.			% se deja intacto y se procesa el resto


% --- reversa/2(+Lista,-Reversa) -----------------------------------------------
%     Reversa es el resultado de invertir los elementos de la Lista

reversa(Lista,Reversa) :-			% usamos una pila (inicialmente vacía) para
   rev(Lista,[],Reversa).			% invertir la lista, y se la pasamos a rev/3


% --- rev/3() ------------------------------------------------------------------

rev([],Reversa,Reversa).			% si la lista está vacío, se devuelve vacío;
							% en otro caso se pone la cabeza de la lista
rev([X|ColaX],Pila,Reversa) :-		% en la pila y se procesa el resto, y se
   rev(ColaX,[X|Pila],Reversa).		% devuelve a reversa/2 la pila


% --- hanoi/4(+N,+De,+A,+Usando) -----------------------------------------------
%     las torres de Hanoi

hanoi(0,_,_,_) :- !.					% (el usuario etiqueta las torres)
							% si no hay discos que mover, no hacer nada
hanoi(N,Origen,Destino,Temp) :-		% para pasar N discos del Origen al Destino,
   M is N-1,					% se resuelve el problema para N-1 discos:
   hanoi(M,Origen,Temp,Destino),	% se pasan los discos del Origen al Destino,
   decir(Origen,Destino),			% se muestra el movimiento, y
   hanoi(M,Temp,Destino,Origen).	% se pasa por último el N-ésimo disco

decir(De,A) :-					% este predicado
   write('* '), write(De),		% muestra de dónde
   write(' -> '), write(A), nl.		% hacia dónde se mueve un disco


% --- hanoi2/1(+N) -------------------------------------------------------------
%     otra versinn de las torres de Hanoi

hanoi(N) :-					% (etiquetamos las torres: izq,centro,der)
   mover(N,izq,centro,der).		% y mover se hacer cargo de resolverlo

mover(0,_,_,_) :- !.				% si no hay discos que mover, no hacer nada

mover(N,A,B,C) :-				% para pasar N discos de A a B,
   M is N-1,					% se resuelve el problema para N-1 discos:
   mover(M,A,C,B),				% se pasan los discos de A a B,
   decir(A,B),					% se muestra el movimiento, y
   mover(M,C,B,A).				% se pasa por último el N-ésimo disco


% --- ancestro(?X,?Y) –---------------------------------------------------------
%     +X,+Y: determina si Y es ancestro de X
%     +X,-Y: encuentra los ancestros de X
%     -X,+Y: encuentra de quiénes es ancestro Y

ancestro(X,Y) :-				% el padre y la madre de X son sus ancestros
   padre(X,Y) ; madre(X,Y).
ancestro(X,Y) :-				% y también lo son los ancestros de ellos
   ancestro(X,Z), ancestro(Z,Y).


% --- afirmaTodo/1(+ListaDeCláusulas) ------------------------------------------
%     afirma todos los hechos y reglas de la lista (sin revisión de sintaxis)

afirmaTodo([]).					% nada más que afirmar
afirmaTodo([C|Rc]) :- X = C,		% guardar el patrón C en X temporalmente
   afirmalo(X), afirmaTodo(Rc).		% afirmar X y recurrir sobre la cola
afirmalo(X) :- assert(X).


% --- retractaTodo/1(+Termino) -------------------------------------------------
%     retracta todos los hechos que se unifiquen con el término T

retractaTodo(T) :-
   retractall(T).


% --- El operador de corte (!, "cut")
%     Se utiliza para evitar la retroacción a una zona específica del árbol Y/O
%     de búsqueda del programa Prolog
%
% --- ejemplo (la negación como falla):

negacion(M,fail) :- M, !.			% si la meta M es verdadera, falla
negacion(_,true).				% en otro caso (es falsa), sale con éxito

negacion2(M,fail) :- M.
negacion2(_,true).
% ------------------------------------------------------------------------------
%
% --- Tarea programada 1: Prolog
%
%     Resuelva los problemas a continuación. Para cada predicado, provea la
%     documentación estándar de Prolog, incluyendo:
%
%     *  La descripción general del predicado (nombre, parámetros)
%     *  Ejemplos de uso del predicado
%
%     Fecha límite de entrega: jueves 17 de septiembre de 2015
%
%
% --- 1. Combinando los diseños de los predicados 'nesimo' y 'miembro', programe
%        un predicado 'posicion/3(+O,+L,-P)' que devuelva la posición P de la
%        primera instancia del objeto O en la lista L. Nota: este predicado
%        sería equivalente al uso de nesimo así: nesimo(-P,+L,+O), donde el
%        predicado devolvería en P la posición del objeto O en la lista L
%
%        Nota: la definición que usted debe elaborar, si se fuerza al
%        backtracking, debe devolver todas soluciones al problema, es decir, las
%        posiciones de todas las instancias del objeto que se encuentren en la
%        lista.

% ---  posicion/3(+O,+L,-P) ---------------------------------------------------
%      Devuelva la posición P de la primera instancia del objeto O en la lista L
%      Esta implementacion no fuerza backtracking
%
% --- ejemplo: posicion(c, [a,b,c,d,e], P).  -> 3

posicion(O, [O|_], 1).
posicion(O, [_|C], P) :-
   pos(2, O, C, P).

pos(I, O, [O|_], P) :-
   P is I.

pos(I, O, [_|C], P) :-
   J is I + 1, pos(J, O, C, P).

%
% --- 2. En forma similar al problema anterior, programe un predicado
%        subarbol/3(+O,+A,-S) que devuelva la sublista de S en la que se
%        encuentre la primera instancia del objeto O en la lista A
%


% ---  subarbol/3(+O,+A,-S) ---------------------------------------------------
%      Devuelva la sublista S en la que encuentra la primera instancia del
%      objeto O en la lista A
%
% --- ejemplo: subarbol(d, [a, b, [d, e, f], g], S).  -> S = [d, e, f] ;

subarbol(O, [O|Ok], [O|Ok]) :- !. % caso coincide con raiz
subarbol(O, [_|[O|_]], [O]) :- !. % coincide con el segundo elemento

subarbol(O, [R|[P|Pk]], S) :-
  is_list(R), subarbol(O, R, S); % si la raiz es lista llame recurs
  is_list(P), subarbol(O, P, S); % si el segundo elemento es lista llame recur
  subarbol(O, Pk, S), !. % iteracion en la lista del arbol

% --- 3. Explique en detalle qué hace el intérprete Prolog, paso a paso con la
%        meta miembro(X,[a,b,c,d,e]).
%
% Cuando se ejecuta miembro(X, [a,b,c,d,e). este es unificable con la primera 
% regla de miembro, osea X se unifica con a, pero como no hay corte de backtracking 
% el intérprete unifica la proposición con la segunda regla, entonces hace llamado 
% recursivo con X y la lista [b,c,d,e]. Luego se vuelve a disparar la primera regla 
% y X se unifica con b, pero como hay backtracking se dispara la segunda regla y se 
% llama recursivamente con [c,d,e]. Esto sucede de manera similar con el resto de la lista.
%
% --- 4. En el predicado 'maximo/2', ¿dónde podríamos incluir un corte para
%        evitar múltiples soluciones?
%
% El predicado maximo/2 no presenta multiples soluciones.
% Si se ejecuta maximo([2,54,6], M). La respuesta del interprete es
% M = 54.
%
% --- 5. Modifique el predicado 'negacion/2' eliminando el operador de corte,
%        y explique qué sucede.
%
% Hice el predicado negacion2/2 que no tiene el corte y cuando se ejecuta
% por ejemplo negacion2(2=2, X). el interprete indica que X = fail y luego por 
% backtracking indica X = true.
%
% --- 6. Indique las llamadas al predicado assert necesarias para añadir el
%        predicado sub/3.
%
% sub(X,[X|Rx],[X|Rx]).
% sub(X,[_|R],Z) :-  sub(X,R,Z).
%
%
% Para añadir estas reglas se usan estas llamas a assert
% assert(sub(X,[X|Rx],[X|Rx])).
% assert(sub(X,[_|R],Z) :- sub(X,R,Z)).
%
%
% --- 7. Use retractaTodo/1 para eliminar todos los hechos de sub/3. ¿Elimina
%        todas las cláusulas del predicado? Modifique retractaTodo/1 para que
%        elimine también todas las reglas.
%
% Originalmente no se eliminan las clausulas pero modificando el predicado
% retractaTodo estas llamadas borran el predicado sub/3
%
% retractaTodo(sub(X,[X|Rx],[X|Rx])).
% retractaTodo(sub(X,[_|R],Z) :- sub(X,R,Z)).
%
% --- 8. Programe los predicados de manejo de conjuntos siguientes: union/3,
%        intereseccion/3, diferencia/3, y potencia/2.
%
%        union/3(+A,+B,-C) -> C es la unión de A y B
%        interseccion/3(+A,+B,-C) -> C es la intersección de A y B
%        diferencia/3(+A,+B,-C) -> C es la diferencia de A y B
%        potencia/2(+A,-P) -> P es el conjunto potencia de A
%
% ------------------------------------------------------------------------------


% ---  union/3(+A,+B,-C) -------------------------------------------------------
%      Devuelva la unión de los conjuntos A y B en el conjunto C
%
% --- ejemplo: union([c, d], [d,a,b], C).  -> C = [c, d, a, b]

union(A, [], A).     % casos donde uno de los dos conjuntos es vacio
union([], B, B).     % ademas este es el caso donde se va "vaciando" a A

union([A|Ak], B, C) :-
   member(A,B), union(Ak, B, C).    % si la cabeza de A ya está en B, continúe
union([A|Ak], B, [A|Ck]) :-
   not(member(A, B)), union(Ak, B, Ck). % si la cabeza de A no está en B, unifíquela con C


% ---  interseccion/3(+A,+B,-C) -------------------------------------------------
%      Devuelva la insersección de los conjuntos A y B en el conjunto C
%
% --- ejemplo: interseccion([a,g,h,b,c], [b,c], C). -> C = [b, c] .

interseccion(_, [], []).
interseccion([], _, []).
interseccion([], _, _).

interseccion([A|Ak], B, C) :-
   not(member(A,B)), interseccion(Ak, B, C).
interseccion([A|Ak], B, [A|Ck]) :-
   member(A, B), interseccion(Ak, B, Ck).


% ---  diferencia/3(+A,+B,-C) ---------------------------------------------------
%      Devuelva la diferencia de los conjuntos A y B en el conjunto C
%
% --- ejemplo: diferencia([a,g,h,b,c], [b,c], C). -> C = [a, g, h]

diferencia(A, [], A).     % si B es vacío la diferencia es todo A
diferencia([], _, []).    % condiciones de parada
diferencia([], _, _).

diferencia([A|Ak], B, C) :-
   member(A,B), diferencia(Ak, B, C).    % si la cabeza de A ya está en B, continúe
diferencia([A|Ak], B, [A|Ck]) :-
   not(member(A, B)), diferencia(Ak, B, Ck). % si la cabeza de A no está en B, unifíquela con C


% ---  subconjunto/2(-R,+L) ---------------------------------------------------
%      Devuelve todos los subconjuntos de la lista L y los unifica por medio de
%      backtracking
%
% --- ejemplo: subconjunto(X, [a,b,c]). ->
% X = [a, b, c] ;
% X = [a, b] ;
% X = [a, c] ;
% X = [a] ;
% X = [b, c] ;
% X = [b] ;
% X = [c] ;
% X = [].

subconjunto([],[]).
subconjunto([S|Sk], [S|Tk]) :- subconjunto(Sk,Tk). % unifica las cabezas
subconjunto(S, [_|Tk]) :- subconjunto(S,Tk).

% ---  potencia/2(+C, -R) ---------------------------------------------------
%      Devuelve el conjunto potencia de la lista C y lo devuelve como otra lista en R.
%      Este predicado utliza el predicado subconjunto/2 y lo almacena en la lista por
% medio del predicado findall ya definido por swi-prolog

% --- ejemplo: potencia([a,b,c], X). -> X = [[a, b, c], [a, b], [a, c], [a], [b, c], [b], [c], []].
potencia(C, R) :- findall(A, subconjunto(A, C), R). % la variable libre A es auxiliar
