

% initMyData(+NBoxesToPick, -ProgramDataInitial).
% starea interna a robotului presupune: coordonatele de la momentul actual, numarul de cutii pe care le are de carat(NBoxesToPick),
%									    un flag care il informeaza in cazul in care cara o cutie(la inceput este false pentru ca nu cara nicio cutie),
%										numarul maxim de mutari executate spre est(care este egal cu numarul maxim de mutari efectuate spre sud),
%										numarul maxim de mutari executate spre vest(care este egal cu numarul maxim de mutari efectuate spre nord)
%										in cazul unei parcurgeri in spirala a spatiului, numarul de mutari executate intr-o anumita directie,
%										precum si directia curenta


initMyData(NBoxesToPick, [(0,0), NBoxesToPick, false, 1, 2, 0, e]).

% perform(+ProgramData, +ContainsBox, -Action, -ProgramDataUpdated)

% (X, Y) - coordonatele pe care se afla robotul in momentul actual
% (XNext, YNext) - coordonatele pe care se va afla robotul dupa momentul actual

% robotul se afla in origine si a livrat toate cutiile, deci a terminat treaba
perform([(0,0), 0, false, _, _, _, _], _, done, [(0, 0), 0, false, _, _, _, _]).


%% mutari pentru a cauta cutia
% caut o cutie mergand in spirala spre est

% daca nu s-a efectuat numarul maxim de mutari spre est, robotul se muta spre est 		
perform([(X, Y), NBoxesToPick, false, SEMoves, NVMoves, NumberOfMoves, e], false, move(east),
		[(XNext, YNext), NBoxesToPick, false, SEMoves, NVMoves, NoMoves, e]) :- 
		NumberOfMoves =\= SEMoves, XNext is X + 1, YNext is Y, NoMoves is NumberOfMoves + 1, !.
		
% daca s-a efectuat numarul maxim de mutari spre est, robotul coteste spre sud		
perform([(X, Y), NBoxesToPick, false, EastMoves, WestMoves, EastMoves, e], false, move(south),
		[(XNext, YNext), NBoxesToPick, false, EM, WM, 0, s]) :- 
		XNext is X, YNext is Y - 1, EM is EastMoves , WM is WestMoves , ! .	
% sud
% daca nu s-a efectuat numarul maxim de mutari spre sud, robotul se muta spre sud	
perform([(X, Y), NBoxesToPick, false, SouthMoves, WestMoves, NumberOfMoves, s], false, move(south),
		[(XNext, YNext), NBoxesToPick, false, SouthMoves, WestMoves, NoMoves, s]) :- 
		NumberOfMoves =\= SouthMoves, XNext is X , YNext is Y - 1, NoMoves is NumberOfMoves + 1, !.

% daca s-a efectuat numarul maxim de mutari spre sud, robotul coteste spre vest
perform([(X, Y), NBoxesToPick, false, SouthMoves, WestMoves, SouthMoves, s], false, move(west),
		[(XNext, YNext), NBoxesToPick, false, EM, WM, 0, v]) :- 
		XNext is X - 1 , YNext is Y , EM is SouthMoves  , WM is WestMoves, ! .

% vest
% daca nu s-a efectuat numarul maxim de mutari spre vest, robotul se muta spre vest 	
perform([(X, Y), NBoxesToPick, false, EastMoves, WestMoves, NumberOfMoves, v], false, move(west), 
		[(XNext, YNext), NBoxesToPick, false,EastMoves, WestMoves, NoMoves, v]) :- 
		NumberOfMoves =\= WestMoves, XNext is X - 1 , YNext is Y , NoMoves is NumberOfMoves + 1, !.

% daca s-a efectuat numarul maxim de mutari spre vest, robotul coteste spre nord
perform([(X, Y), NBoxesToPick, false, EastMoves, WestMoves, WestMoves, v], false, move(north),
		[(XNext, YNext), NBoxesToPick, false, EM, WM, 0, n]) :- 
		XNext is X , YNext is Y + 1, EM is EastMoves , WM is WestMoves , !.
		
% nord
% daca nu s-a efectuat numarul maxim de mutari spre nord, robotul se muta spre nord 	
perform([(X, Y), NBoxesToPick, false, EastMoves, WestMoves, NumberOfMoves, n], false, move(north),
		[(XNext, YNext), NBoxesToPick, false, EastMoves, WestMoves, NoMoves, n]) :- 
		NumberOfMoves =\= WestMoves, XNext is X , YNext is Y + 1, NoMoves is NumberOfMoves + 1, !.

% daca s-a efectuat numarul maxim de mutari spre nord, robotul coteste spre est
perform([(X, Y), NBoxesToPick, false, EastMoves, NorthMoves, NorthMoves, n], false, move(east),
		[(XNext, YNext), NBoxesToPick, false, EM, WM, 0, e]) :- 
		XNext is X + 1 , YNext is Y, EM is EastMoves + 2 , WM is NorthMoves + 2 , !.
		

%% am gasit o cutie, fac pickBox
perform([(X, Y), NBoxesToPick, false, _, _, _, _], true, pickBox, [(X, Y), NBoxesToPick, true, _, _, _, _]) :- !. 

	
%% ma intorc in origine cu o cutie 
%% merg pana cand coordonata X e 0, apoi pana cand coordonata Y e 0 si ajung in origine

perform([(X, Y), NBoxesToPick, true, _, _, _, _], _, moveWithBox(west), [(XNext, Y), NBoxesToPick, true, _, _, _, v]) :- 
	 X > 0, XNext is X - 1, !.
perform([(X, Y), NBoxesToPick, true, _, _, _, _], _, moveWithBox(east), [(XNext, Y), NBoxesToPick, true, _, _, _, e]) :- 
	 X < 0, XNext is X + 1, !.
perform([(X, Y), NBoxesToPick, true, _, _, _, _], _, moveWithBox(north), [(X, YNext), NBoxesToPick, true, _, _, _, _]) :- 
	 Y < 0, YNext is Y + 1, !.
perform([(X, Y), NBoxesToPick, true, _, _, _, _], _, moveWithBox(south), [(X, YNext), NBoxesToPick, true, _, _, _, _]) :- 
	 Y > 0, YNext is Y - 1, !.

%livrez cutia si scad numarul de cutii pe care le mai am de livrat
perform([(0, 0), NBoxesToPick, true, _, _, _, _], _, deliverBox, [(0, 0), BoxesLeftToPick, false, 1, 2, 0, e]):- BoxesLeftToPick is NBoxesToPick - 1, !.	 
 
% pentru bonus:
% perform(+ProgramData, +ContainsBox, +AvailableDirections,
%                                   -Action, -ProgramDataUpdated)
perform(_, _, _, done, _).
