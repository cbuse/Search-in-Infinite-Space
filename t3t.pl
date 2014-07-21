
:- ensure_loaded('t3.pl').

% t3test(?ID, -NBoxesToDeliver, -BoxPositions, -LimitOnNumberOfActions)
% t3testBonus(?ID, -NBoxesToDeliver, -BoxPositions,
% -ObstaclePositions, -LimitOnNumberOfActions)

% no required boxes - all sumed up to 1 point
t3test(a1, 0, [], 500).
t3test(a2, 0, [(1, 0)], 500).
t3test(a3, 0, [(0, 0), (1, 0)], 500).

% required boxes are in origin - all summed up to 1 point
t3test(b1, 1, [(0, 0), (2, 2), (1, 2), (5, 5), (0, 0)], 500).
t3test(b2, 2, [(0, 0), (2, 2), (1, 2), (5, 5), (0, 0)], 500).

% required boxes are east only - 1 point each
t3test(c1, 1, [(0, 1), (2, 0), (1, 2), (5, 5), (5, 0)], 500).
t3test(c2, 2, [(0, 1), (2, 0), (1, 2), (5, 5), (5, 0)], 500).

% required boxes are in various positions - 1 point each
t3test(d1, 1, [(1, 2), (5, 5), (2, 2)], 500).
t3test(d2, 2, [(2, 2), (1, 2), (5, 5)], 500).
t3test(d3, 3, [(0, 0), (2, 2), (1, 2), (5, 5), (0, 0)], 500).
t3test(d4, 4, [(0, 0), (2, 2), (1, 2), (5, 5), (0, 0)], 500).
t3test(d5, 5, [(0, 0), (2, 2), (1, 2), (5, 5), (0, 0)], 500).

% 1 point each
t3test(e1, 2, [(0, 2), (0, 4)], 500).
t3test(e2, 4, [(-2, -2), (-2, 2), (2, 2), (2, -2)], 200).

% .33, .33, .34, .33, .33, .34
t3testBonus(bonus1, 2, [(0, 2), (0, 4)], [(0, 1), (-1, -1)], 500).
t3testBonus(bonus2, 2, [(0, 4), (-5, 5)], [(-1, 3), (1, 3), (3, -1), (3, 1), (-1, -3), (1, -3), (-3, 1), (-3, -1)], 500).
t3testBonus(bonus3a, 2, [(0, -1), (0, 1)], [(1, 0), (-1, 0)], 500).
t3testBonus(bonus3b, 2, [(1, 0), (-1, 0)], [(0, -1), (0, 1)], 500).
t3testBonus(bonus4, 4, [(-3, 3), (-1, 3), (1, 3), (3, 3)], [(-2, 3), (0, 3), (2, 3)], 500).
t3testBonus(bonus5, 2, [(0, 3), (2, 3), (-2, 3), (0, -3), (2, -3), (-2, -3)], [(-1, 3), (1, 3), (3, -1), (3, 1), (-1, -3), (1, -3), (-3, 1), (-3, -1)], 500).

% notverb.
notverb :- fail.

% runs one test (found by ID)
t3test(ID) :-
	t3test(ID, N, Boxes, StepsLimit),
	t3testone(ID, N, Boxes, [], StepsLimit).
t3test(ID) :-
	t3testBonus(ID, N, Boxes, Obstacles, StepsLimit),
	t3testone(ID, N, Boxes, Obstacles, StepsLimit).
% MAIN TEST Routine: runs all tests and reports at the end.
t3testallinone :-
	findall(ID, t3test(ID, _, _, _), AllTests), length(AllTests, LT),
	findall(ID, (t3test(ID, N, Boxes, Limit), t3testone(ID, N, Boxes, [], Limit)), L),
	length(L, LS), diff(AllTests, L, LF),
	format("~n~nResult: ~w / ~w tests succeeded: ~w. tests failed: ~w.~n", [LS, LT, L, LF])
	.

diff([], _, []).
diff([E | T], L, T1) :- member(E, L), !, diff(T, L, T1).
diff([E | T], L, [E | T1]) :- diff(T, L, T1).
% ADDITIONAL COMPLETE TEST ROUTINE: runs tests, as separate solutions
t3testall :-
	t3test(ID, N, Boxes, StepsLimit),
	t3testonenofail(ID, N, Boxes, [], StepsLimit).
t3testallbonus :-
	t3testBonus(ID, N, Boxes, Obstacles, StepsLimit),
	t3testonenofail(ID, N, Boxes, Obstacles, StepsLimit).


% run one test: id of the test, number of boxes to deliver,
%  positions of boces, limit on number of actions allowed
t3testonenofail(ID, N, Boxes, Obstacles,StepsLimit) :- (t3testone(ID, N, Boxes, Obstacles, StepsLimit), ! ; true).
t3testone(ID, N, Boxes, Obstacles, StepsLimit) :-
	(   notverb, ! ; format("~n~n~n") ),
	format('== test [~w]: ', [ID]),
	format('[~w] boxes in positions ~w', [N, Boxes]),
	(Obstacles=[_|_], !, (   notverb, ! ; format("~n==~n") ), format(' obstacles: ~w', [Obstacles]); true),
	(   notverb, ! ; format("~n==~n") ),
	(   initMyData(N, ProgramData), ! ; format('~nError: Predicate initMyData returned false~n'), !, fail),
	(   notverb, ! ; format("Simulated position / box here / carries box / program data / available ~t~72| ....action chosen~n")  ),
	(   t3testonline(N, Boxes, Obstacles, (0, 0), false, ProgramData, StepsLimit, []),
	    (	 notverb ; format('test completed successfully ==================~n') ),
            !
	    ;
	    format('============================ test failed ~n'),
            fail
	)
	.

% requires actions and tests that actions are performed correctly.
% arguments: number of boxes to deliver, positions of boxes,
%  current simulated position, is robot carrying a box,
%  current data to be passed to the perform predicate,
%  number of left allowed actions, history (reversed) of actions.
t3testonline(_, _, _, _, _, _, 0, _) :- !, format('Steps limit reached. The problem should be solved with fewer actions.'), fail.
t3testonline(N, Boxes, Obstacles, CPos, CarriesBox, ProgramData, StepsLeft, H) :-
	StepsLeft > 0,
	(   member(CPos, Boxes), !, HasBox = true
	    ;
	    HasBox = false
	),
	t3availables(CPos, Obstacles, Availables),
	(   notverb, ! ;
	    CPos = (X, Y), t3availablereplace(Availables, AvailOut),
	    format("[~w~3|,~w~6|]~7|[~w]~15|[~w]~24|[~w]~w~t~72| ....", [X, Y, HasBox, CarriesBox, ProgramData, AvailOut]),
            format('Getting action to perform... ')
	), !,
	(	Obstacles=[_|_], !, t3testperform(ProgramData, HasBox, Availables, Action, ProgramDataUpdated)
		;
		t3testperform(ProgramData, HasBox, [], Action, ProgramDataUpdated)
	),
	StepsMinus is StepsLeft - 1,
	(   Action == done, !, t3testfinalize((HasBox, ProgramData), N, Boxes, CPos, CarriesBox, H)
	    ;
	    t3testact((HasBox, ProgramData), N, Boxes, Availables, CPos, CarriesBox, Action, NOut, BoxesOut, CPosOut, CarriesBoxOut), !,
	    t3testonline(NOut, BoxesOut, Obstacles, CPosOut, CarriesBoxOut, ProgramDataUpdated, StepsMinus, [Action | H])
	).
t3testonline(_, _, _, _, _, _, _, _) :- format("Error.~n"), fail.

t3testperform(ProgramData, PosHasBox, [], Action, ProgramDataUpdated) :-
	!, perform(ProgramData, PosHasBox, Action, ProgramDataUpdated), !,
	(   notverb, ! ; format('[~w]~n', [Action])  )
	.
t3testperform(ProgramData, PosHasBox, Availables, Action, ProgramDataUpdated) :-
	perform(ProgramData, PosHasBox, Availables, Action, ProgramDataUpdated), !,
	(   notverb, ! ; format('[~w]~n', [Action])  )
	.
t3testperform(ProgramData, PosHasBox, Availables, _, _) :-
	format("~nSimulated position / box here / carries box / program data / available ~t~72| ....action chosen~n"),
	format("[--~3|,--~6|]~7|[~w]~15|[--]~24|[~w]~w~t~72| ....[--]~m", [PosHasBox, ProgramData, Availables]),
	format("ERROR: Perform predicate failed.~n"),
	fail.


t3testact(DD, N, Boxes, Availables, (X, Y), false, move(Direction), N, Boxes, (XO, YO), false) :-
	(   member(Direction, Availables), t3testdelta(Direction, DX, DY), !, XO is X + DX, YO is Y + DY
	    ;
	    format("~nSimulated position / box here / carries box / program data / available ~t~72| ....action chosen~n"), DD = (HB, PD),
	    format("[~w~3|,~w~6|]~7|[~w]~15|[~w]~24|[~w]~w~t~72| ....[~w]~n", [X, Y, HB, false, PD, Availables, move(Direction)]),
	    format("ERROR: Direction ~w from ~w is invalid or unavailable.~n", [Direction, (X, Y)]),
            !, fail)
	.
t3testact(DD, _, _, Availables, (X, Y), true, move(Direction), _, _, _, _) :- !,
	format("~nSimulated position / box here / carries box / program data / available ~t~72| ....action chosen~n"), DD = (HB, PD),
	format("[~w~3|,~w~6|]~7|[~w]~15|[~w]~24|[~w]~w~t~72| ....[~w]~n", [X, Y, HB, true, PD, Availables, move(Direction)]),
	format("ERROR: Robot is carrying a box. Permitted operations are moveWithBox and deliverBox."),
        fail.

t3testact(DD, N, Boxes, Availables, (X, Y), true, moveWithBox(Direction), N, Boxes, (XO, YO), true) :-
	(   member(Direction, Availables), t3testdelta(Direction, DX, DY), !, XO is X + DX, YO is Y + DY
	    ;
	    format("~nSimulated position / box here / carries box / program data / available ~t~72| ....action chosen~n"), DD = (HB, PD),
	    format("[~w~3|,~w~6|]~7|[~w]~15|[~w]~24|[~w]~w~t~72| ....[~w]~n", [X, Y, HB, true, PD, Availables, moveWithBox(Direction)]),
	    format("ERROR: Direction ~w from ~w is invalid or unavailable.~n", [Direction, (X, Y)]),
            !, fail)
	.
t3testact(DD, _, _, Availables, (X, Y), false, moveWithBox(Direction), _, _, _, _) :- !,
	format("~nSimulated position / box here / carries box / program data / available ~t~72| ....action chosen~n"), DD = (HB, PD),
	format("[~w~3|,~w~6|]~7|[~w]~15|[~w]~24|[~w]~w~t~72| ....[~w]~n", [X, Y, HB, false, PD, Availables, moveWithBox(Direction)]),
	format("ERROR: Robot is not carrying a box. Permitted operations are move and pickBox."),
        fail.

t3testact(_, N, Boxes, _, CPos, false, pickBox, N, BoxesOut, CPos, true) :-
	N > 0, member(CPos, Boxes), !,
	t3testr(CPos, Boxes, BoxesOut).
	%% format("====== box picked; boxes left to pick: ~w ~n", [BoxesOut])

t3testact(DD, _, _, Availables, (X, Y), true, pickBox, _, _, _, _) :- !,
	format("~nSimulated position / box here / carries box / program data / available ~t~72| ....action chosen~n"), DD = (HB, PD),
	format("[~w~3|,~w~6|]~7|[~w]~15|[~w]~24|[~w]~w~t~72| ....[~w]~n", [X, Y, HB, true, PD, Availables, pickBox]),
	format("ERROR: Robot is currently carrying a box. Permitted operations are moveWithBox and deliverBox."),
        fail.
t3testact(DD, _, Boxes, Availables, CPos, CarriesBox, pickBox, _, _, _, _):-
        \+ member(CPos, Boxes), !,
	format("~nSimulated position / box here / carries box / program data / available ~t~72| ....action chosen~n"), DD = (HB, PD), CPos = (X, Y),
	format("[~w~3|,~w~6|]~7|[~w]~15|[~w]~24|[~w]~w~t~72| ....[~w]~n", [X, Y, HB, CarriesBox, PD, Availables, pickBox]),
        format("ERROR: No box to pick in position ~w.~n", [CPos]),
        fail.
t3testact(DD, 0, _, Availables, CPos, CarriesBox, pickBox, _, _, _, _) :-
	format("~nSimulated position / box here / carries box / program data / available ~t~72| ....action chosen~n"), DD = (HB, PD), CPos = (X, Y),
	format("[~w~3|,~w~6|]~7|[~w]~15|[~w]~24|[~w]~w~t~72| ....[~w]~n", [X, Y, HB, CarriesBox, PD, Availables, pickBox]),
	format("ERROR: No reason to pick any more boxes; deliveries are completed.~n"),
        fail.

t3testact(_, N, Boxes, _, (0, 0), true, deliverBox, NOut, Boxes, (0, 0), false) :-
	N > 0, NOut is N - 1, !.
	%%format("====== box delivered; boxes left to deliver: [~w]~n", NOut)

t3testact(DD, _, _, Availables, (X, Y), false, deliverBox, _, _, _, _) :- !,
	format("~nSimulated position / box here / carries box / program data / available ~t~72| ....action chosen~n"), DD = (HB, PD),
	format("[~w~3|,~w~6|]~7|[~w]~15|[~w]~24|[~w]~w~t~72| ....[~w]~n", [X, Y, HB, false, PD, Availables, pickBox]),
	format("ERROR: Robot is not carrying a box. Permitted operations are move and pickBox."),
	fail.

t3testact(DD, _, _, Availables, (X, Y), CarriesBox, deliverBox, _, _, _, _):-
        (X =\= 0 ; Y =\= 0), !,
	format("~nSimulated position / box here / carries box / program data / available ~t~72| ....action chosen~n"), DD = (HB, PD),
	format("[~w~3|,~w~6|]~7|[~w]~15|[~w]~24|[~w]~w~t~72| ....[~w]~n", [X, Y, HB, CarriesBox, PD, Availables, deliverBox]),
	format("ERROR: Unable to deliver boxes at other positions than 0,0; current position: ~w.", [(X, Y)]),
        fail.

t3testact(DD, _, _, Availables, (X, Y), CarriesBox, Action, _, _, _, _) :-
	not(member(Action, [done, pickBox, deliverBox, move(_), moveWithBox(_)])),
	!,
	format("~nSimulated position / box here / carries box / program data / available ~t~72| ....action chosen~n"), DD = (HB, PD),
	format("[~w~3|,~w~6|]~7|[~w]~15|[~w]~24|[~w]~w~t~72| ....[~w]~n", [X, Y, HB, CarriesBox, PD, Availables, Action]),
        format("ERROR: Action [~w] does not exist. Check your spelling and case.~n", [Action]),
        fail.

t3testact(DD, _, _, Availables, (X, Y), CarriesBox, Action, _, _, _, _):-
        !,
	format("~nSimulated position / box here / carries box / program data / available ~t~72| ....action chosen~n"), DD = (HB, PD),
	format("[~w~3|,~w~6|]~7|[~w]~15|[~w]~24|[~w]~w~t~72| ....[~w]~n", [X, Y, HB, CarriesBox, PD, Availables, Action]),
        format("ERROR: Action is invalid. No more information on this error.~n"),
        fail.
t3testa([(east,1,0), (west,-1,0),(north,0,1),(south,0,-1)]).
t3testdelta(D, D1, D2) :- t3testa(A),member((D,D1,D2),A),!.
t3testdeltA(D, D1, D2) :- t3testa(A),member((D,D1,D2),A).
t3testr(E, [H | L], LO) :- (H=E,!, LO=L; !, t3testr(E, L, LO1), LO=[H|LO1]).
t3availables((X, Y), Obs, A) :- findall(D, (t3testdeltA(D,DX,DY),TX is X+DX,TY is Y+DY,not(member((TX,TY),Obs))), A).
t3availablereplace([], []).
t3availablereplace([north | L], ['^' | LO]) :- t3availablereplace(L, LO).
t3availablereplace([south | L], ['v' | LO]) :- t3availablereplace(L, LO).
t3availablereplace([west | L], ['>' | LO]) :- t3availablereplace(L, LO).
t3availablereplace([east | L], ['<' | LO]) :- t3availablereplace(L, LO).


t3testfinalize(_, 0, _, (0, 0), false, H) :- reverse(H, HR),
	(   notverb, !; format("Double checking track...") ), t3testCH(HR, (0,0)), !, format(" ...Completed.~n").
t3testfinalize(DD, _, _, (X, Y), false, _) :- X =\= 0, Y =\= 0, !,
	format("~nSimulated position / box here / carries box / program data / available ~t~72| ....action chosen~n"), DD = (HB, PD),
	format("[~w~3|,~w~6|]~7|[~w]~15|[~w]~24|[~w]--~t~72| ....[~w]~n", [X, Y, HB, false, PD, done]),
	format("ERROR: Cannot complete solution now; Robot is not in origin (position is ~w,~w).~n", [X, Y]), fail.
t3testfinalize(DD, _, _, (X, Y), true, _) :- !,
	format("~nSimulated position / box here / carries box / program data / available ~t~72| ....action chosen~n"), DD = (HB, PD),
	format("[~w~3|,~w~6|]~7|[~w]~15|[~w]~24|[~w]--~t~72| ....[~w]~n", [X, Y, HB, true, PD, done]),
	format("ERROR: Cannot complete solution now; Robot is still carrying a box.~n"),
	fail.
t3testfinalize(DD, N, _, (X, Y), CarriesBox, _) :- N > 0, !,
	format("~nSimulated position / box here / carries box / program data / available ~t~72| ....action chosen~n"), DD = (HB, PD),
	format("[~w~3|,~w~6|]~7|[~w]~15|[~w]~24|[~w]--~t~72| ....[~w]~n", [X, Y, HB, CarriesBox, PD, done]),
	format("ERROR: Solution not complete; There are ~w boxes left to deliver.~n", [N]),
	fail.
t3testfinalize(DD, _, _, (X, Y), CarriesBox, _) :-
	format("~nSimulated position / box here / carries box / program data / available ~t~72| ....action chosen~n"), DD = (HB, PD),
	format("[~w~3|,~w~6|]~7|[~w]~15|[~w]~24|[~w]--~t~72| ....[~w]~n", [X, Y, HB, CarriesBox, PD, done]),
	format("ERROR: Internal or format error when trying to complete solution.~n"),
	fail.

t3testCH([], (0,0)) :- (notverb, ! ; format("...complete.~n") ).
t3testCH([], (X, Y)) :- format("double check failed: moves completed at [~w,~w].~n", [X,Y]), fail.
t3testCH([M | H], (X, Y)) :- ( M = move(D) ; M = moveWithBox(D) ), t3testdelta(D, DX, DY), X1 is X + DX, Y1 is Y + DY, t3testCH(H, (X1, Y1)), !.
t3testCH([M | H], (X, Y)) :- ( M = pickBox ; M = deliverBox ), t3testCH(H, (X, Y)), !.
t3testCH(H, (X, Y)) :- format("double check failed at position [~w,~w] with remaining moves: ~w.~n", [X, Y, H]), fail.









