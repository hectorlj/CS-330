teacher(appleton).
teacher(gross).
teacher(knight).
teacher(mcevoy).
teacher(parnell).

subject(english).
subject(gym).
subject(history).
subject(math).
subject(science).

state(california).
state(florida).
state(maine).
state(oregon).
state(virginia).

activity(antiquing).
activity(camping).
activity(sightseeing).
activity(spelunking).
activity(waterskiing).

solve :- 
	subject(AppletonSub), subject(GrossSub), subject(KnightSub), subject(McevoySub), subject(ParnellSub),
		all_different([AppletonSub, GrossSub, KnightSub, McevoySub, ParnellSub]),
	state(AppletonState), state(GrossState), state(KnightState), state(McevoyState), state(ParnellState),
		all_different([AppletonState, GrossState, KnightState, McevoyState, ParnellState]),
	activity(AppletonActivity), activity(GrossActivity), activity(KnightActivity), activity(McevoyActivity), activity(ParnellActivity),
		all_different([AppletonActivity, GrossActivity, KnightActivity, McevoyActivity, ParnellActivity]),
	Solutions = [
					[appleton, AppletonState, AppletonSub, AppletonActivity],
					[gross, GrossState, GrossSub, GrossActivity],
					[knight, KnightState, KnightSub, KnightActivity],
					[mcevoy, McevoyState, McevoySub, McevoyActivity],
					[parnell, ParnellState, ParnellSub, ParnellActivity]
				],

	(member([gross, _, math,_], Solutions);
		member([gross, _, science, _], Solutions)),
	(member([gross, florida, _, antiquing], Solutions);
		member([gross, california, _, _], Solutions)),

	member([_, _, science, waterskiing], Solutions),
	(member([_, california, science, waterskiing], Solutions);
		member([_, florida, science, waterskiing], Solutions)),
	member([mcevoy, _, history, _], Solutions),
	(member([mcevoy, maine, history, _], Solutions);
		member([mcevoy, oregon, history, _], Solutions)),

	(member([appleton, virginia, english, _], Solutions);
		member([parnell, virginia, _, spelunking], Solutions)),
	member([parnell, _, _, spelunking], Solutions),

	(\+ member([_, maine, gym, _], Solutions)),
	(\+ member([_, maine, _, sightseeing], Solutions)),

	(\+ member([gross, _, _, camping], Solutions)),
	(member([gross, _, _, antiquing], Solutions);
		member([appleton, _, _, antiquing], Solutions);
		member([parnell, _, _, antiquing], Solutions)),

	tell(appleton, AppletonState, AppletonActivity, AppletonSub),
	tell(gross, GrossState, GrossActivity, GrossSub),
	tell(knight, KnightState, KnightActivity, KnightSub),
	tell(mcevoy, McevoyState, McevoyActivity, McevoySub),
	tell(parnell, ParnellState, ParnellActivity, ParnellSub).



all_different([H|T]) :- member(H,T), !, fail.
all_different([_|T]) :- all_different(T).
all_different([_]).

tell(X, Y, Z, A) :-
	write(X), write(' is going to '),
	write(Y), write(' for '),
	write(Z), write(', and teaches '),
	write(A), write('.'), nl.