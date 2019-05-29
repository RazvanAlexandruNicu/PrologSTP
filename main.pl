% Functie care calculeaza nodul lider.

nodLider([],N, P, N, P) :- !.
nodLider([[N,P]|T], N1, P1, MinNod, MinPrio) :-  P < P1, nodLider(T, N, P, Rez1, Rez2), 
                                                 MinNod = Rez1, MinPrio = Rez2,!.
nodLider([[N,P]|T], N1, P1, MinNod, MinPrio) :-  P >= P1, nodLider(T, N1, P1, Rez1, Rez2),
                                                 MinNod = Rez1, MinPrio = Rez2,!.

% Functie care returneaza o multime de muchii incidente la o multime de noduri.
% Muchii cu o extremitate in 'arborele' curent si o extremitate in afara lui.

muchiiIncidente(Noduri, [], []).
muchiiIncidente(Noduri, [[S,D,C]|T], [[S,D,C]|Rezultat]) :- member(S,Noduri), 
                                                    not(member(D,Noduri)), 
                                                    muchiiIncidente(Noduri, T, Rezultat),!.
muchiiIncidente(Noduri, [[S,D,C]|T], [[S,D,C]|Rezultat]) :- not(member(S,Noduri)), 
                                                    member(D,Noduri), 
                                                    muchiiIncidente(Noduri, T, Rezultat),!.
muchiiIncidente(Noduri, [[S,D,C]|T], Rezultat) :- muchiiIncidente(Noduri, T, Rezultat),!.

% initializez vectorul de distante (distanta 0 pt nodul Root si distanta 9999999 
% pentru celelalte noduri)

initializareDistante(Root, [], VectorDistante, Final) :- Final = VectorDistante, !.
initializareDistante(Root, [Root|T], VectorDistante, Final) :- initializareDistante(Root, T,
	                                                    [[Root,0]|VectorDistante], Final),!.
initializareDistante(Root, [H|T], VectorDistante, Final) :- initializareDistante(Root, T,
	                                                    [[H,9999999]|VectorDistante], Final),!.

% Functie care returneaza nodul prin care se realizeaza legatura unei muchii la 'arborle'
% curent. (Daca in arbore am deja 1,2,3 si leg muchia [3,4], Nodul de legatura este 3,
% iar nodul legat este 4.

nodLegatura(S,D,Noduri, S, D) :- member(S,Noduri),!.
nodLegatura(S,D,Noduri, D, S) :- member(D,Noduri),!.

% Functie care extrage valoarea unei chei dintr-un vector cu elemente [key,value].

distantaNod(Nod, [[Key,Value]|T], Value) :- Nod = Key,!.
distantaNod(Nod, [H|T], Valoare) :- distantaNod(Nod, T, Valoare),!.

% Functie care extrage valoarea unei chei dintr-un vector cu elemente [key,value].

iaPrioritatea(Nod, [[El,Prio]|T], Prio) :- El = Nod, !.
iaPrioritatea(Nod, [[El,Prio]|T], Rez) :- iaPrioritatea(Nod, T, Rez),!.  

% Functie care returneaza muchia de cost minim (cu prioritatea nodului de legatura minima)
% dintr-o multima de muchii date (muchiile incidente la un 'arbore' curent)
% Functia returneaza muchia ce trebuie considerata la pasul curent pentru a forma
% 'arborele' cu distante minime.

muchieMinima([], Noduri, Prioritati, Distante, CostActual, PrioritateActuala, MuchieActuala, 
	         Nod1Actual, Nod2Actual, CostActual, PrioritateActuala,
	         MuchieActuala, Nod1Actual, Nod2Actual) :- !.

muchieMinima([[S,D,C]|T], Noduri, Prioritati, Distante, CostActual, PrioritateActuala,
			MuchieActuala, Nod1Actual, Nod2Actual, CostFinal, PrioritateFinala, 
			MuchieFinala, Nod1Final, Nod2Final) :-
			nodLegatura(S,D,Noduri, Nod, Nod2),
			distantaNod(Nod, Distante, Valoare), CostNou is C + Valoare, CostNou < CostActual,
			iaPrioritatea(Nod, Prioritati, PrioritateNoua),  
			muchieMinima(T, Noduri, Prioritati, Distante, CostNou, PrioritateNoua, [S,D,C],
				         Nod, Nod2, CostFinalRez, PrioFinalRez, MuchieFinalRez,
				         Nod1Rez, Nod2Rez),
			CostFinal = CostFinalRez, PrioritateFinala = PrioFinalRez,
			MuchieFinala = MuchieFinalRez, Nod1Final = Nod1Rez, Nod2Final = Nod2Rez,!.

muchieMinima([[S,D,C]|T], Noduri, Prioritati, Distante, CostActual, PrioritateActuala,
	        MuchieActuala, Nod1Actual, Nod2Actual, CostFinal, PrioritateFinala,
	        MuchieFinala, Nod1Final, Nod2Final) :-
			nodLegatura(S,D,Noduri, Nod, Nod2),
			distantaNod(Nod, Distante, Valoare), CostNou is C + Valoare, CostNou > CostActual,  
			muchieMinima(T, Noduri, Prioritati, Distante, CostActual, PrioritateActuala,
				         MuchieActuala, Nod1Actual, Nod2Actual, CostFinalRez, PrioFinalRez,
				         MuchieFinalRez, Nod1Rez, Nod2Rez),
			CostFinal = CostFinalRez, PrioritateFinala = PrioFinalRez,
			MuchieFinala = MuchieFinalRez, Nod1Final = Nod1Rez, Nod2Final = Nod2Rez,!.

muchieMinima([[S,D,C]|T], Noduri, Prioritati, Distante, CostActual, PrioritateActuala,
	         MuchieActuala, Nod1Actual, Nod2Actual, CostFinal, PrioritateFinala,
	         MuchieFinala, Nod1Final, Nod2Final) :-
			nodLegatura(S,D,Noduri, Nod, Nod2),
			distantaNod(Nod, Distante, Valoare), CostNou is C + Valoare, CostNou =:= CostActual,
			iaPrioritatea(Nod, Prioritati, PrioritateNoua), PrioritateNoua >= PrioritateActuala,
			muchieMinima(T, Noduri, Prioritati, Distante, CostActual, PrioritateActuala,
						 MuchieActuala, Nod1Actual, Nod2Actual, CostFinalRez, PrioFinalRez,
	                     MuchieFinalRez, Nod1Rez, Nod2Rez),
			CostFinal = CostFinalRez, PrioritateFinala = PrioFinalRez,
			MuchieFinala = MuchieFinalRez, Nod1Final = Nod1Rez, Nod2Final = Nod2Rez,!.

muchieMinima([[S,D,C]|T], Noduri, Prioritati, Distante, CostActual, PrioritateActuala,
	         MuchieActuala, Nod1Actual, Nod2Actual, CostFinal, PrioritateFinala,
	         MuchieFinala, Nod1Final, Nod2Final) :-
			nodLegatura(S,D,Noduri, Nod, Nod2),
			distantaNod(Nod, Distante, Valoare), CostNou is C + Valoare, CostNou =:= CostActual,
			iaPrioritatea(Nod, Prioritati, PrioritateNoua), PrioritateNoua < PrioritateActuala,
			muchieMinima(T, Noduri, Prioritati, Distante, CostNou, PrioritateNoua,
				         [S,D,C], Nod, Nod2, CostFinalRez, PrioFinalRez, MuchieFinalRez,
				         Nod1Rez, Nod2Rez),
			CostFinal = CostFinalRez, PrioritateFinala = PrioFinalRez,
			MuchieFinala = MuchieFinalRez, Nod1Final = Nod1Rez, Nod2Final = Nod2Rez,!.

% Functie care returneaza lista nodurilor dintr-un graph.

listaDeNoduri([[N,_]], [N]).  
listaDeNoduri([[N,_]|T], [N|Rezultat1]) :- listaDeNoduri(T, Rezultat1),!.


% Functie care updateaza distanta unui nod dat ca parametru dintr-un vector cu elemente
% [key,value] dat ca parametru

updateDistanta([], Nod, Cost, []).  
updateDistanta([[Nod,C]|T], Nod, Cost, [[Nod,Cost]|Rez1]) :- updateDistanta(T, Nod, Cost, Rez1),!.
updateDistanta([H|T], Nod, Cost, [H|Rez1]) :- updateDistanta(T, Nod, Cost, Rez1),!.  

% Functie care construieste progresiv solutia problemei folosindu-se de functiile
% definite mai sus.

construiesteSolutia([V,E], Distante, ArboreCurent, Solutie, Solutie) :- length(ArboreCurent, S1), 
                                                                        length(V,S2), S1 == S2,!.
construiesteSolutia([V,E], Distante, ArboreCurent, Solutie, Final) :- 
                    muchiiIncidente(ArboreCurent, E, MultimeMuchii) , 
                    muchieMinima(MultimeMuchii, ArboreCurent, V, Distante, 999999, 9999, [],
                    	         0, 0,CostNou, _, _, NodSursa, NodDestinatie),
                    updateDistanta(Distante, NodDestinatie, CostNou, DistanteNoi),
                    construiesteSolutia([V,E], DistanteNoi, [NodDestinatie|ArboreCurent],
                    	                [[NodSursa,NodDestinatie]|Solutie], Final),!.

% Functie care primeste un graf si returneaza nodul Lider si lista
% de muchii continute in arborele de acoperire final.

stp([V,E], Root, Edges) :- nodLider(V, 0, 9999999, Root, _), listaDeNoduri(V,Nodurile), 
                           initializareDistante(Root,Nodurile,[],Distante),
                           construiesteSolutia([V,E],Distante,[Root],[], Edges),!.

% Functie care primeste un graf si returneaza nodul Lider, lista de
% muchii continuta in arborele de acoperire final, un nod sursa,
% un nod destinatie si returneaza calea de la sursa la destinatie
% in arborele de acoperire construit.

drum([V,E], Src, Dst, Root, Edges, Path) :- nodLider(V, 0, 9999999, Root, _),
						listaDeNoduri(V,Nodurile), 
                        initializareDistante(Root,Nodurile,[],Distante),
                        construiesteSolutia([V,E],Distante,[Root],[], Edges), 
                        pathB(Src, Dst, Edges, [], Path),!.

% Functie care verifica daca muchia [X,Y] sau [Y,X] se afla in multimea
% de muchii data ca parametru.
edgeB(X,Y, E) :- member([X,Y], E).
edgeB(X,Y, E) :- member([Y,X], E).

% Functie care construieste drumul de la o sursa la o destinatie pe baza unor
% muchii dintr-un vector de muchii dat ca parametru.

pathB(X,Y, E, Visited, [X,Y]) :- edgeB(X,Y,E), not(member(Y,Visited)).
pathB(X,Y, E, Visited, [X|Path1]) :- edgeB(X,Z,E), not(member(Z, Visited)),
                                pathB(Z,Y, E, [Z|Visited], Path1), not(member(X, Path1)),!.





