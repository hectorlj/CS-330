
customer(hugh).
customer(ida).
customer(jeremy).
customer(leroy).
customer(stella).

rose(cottage).
rose(golden).
rose(mountain).
rose(pink).
rose(sweet).

event(anniversary).
event(charity).
event(retirement).
event(prom).
event(wedding).

item(ballons).
item(candles).
item(placecards).
item(chocolates).
item(streamers).

solve :-
	rose(HughsRose), rose(IdasRose), rose(JeremysRose), rose(LeroysRose), rose(StellasRose),
		all_different([HughsRose, IdasRose, JeremysRose, LeroysRose, StellasRose]),
	event(HughsEvent), event(IdasEvent), event(JeremysEvent), event(LeroysEvent), event(StellasEvent),
		all_different([HughsEvent, IdasEvent, JeremysEvent, LeroysEvent, StellasEvent]),
	item(HughsItem), item(IdasItem), item(JeremysItem), item(LeroysItem), item(StellasItem),
		all_different([HughsItem, IdasItem, JeremysItem, LeroysItem, StellasItem]),

	Solutions = [
				 [hugh, HughsEvent, HughsRose, HughsItem],
	   			 [ida, IdasEvent, IdasRose, IdasItem],
				 [jeremy, JeremysEvent, JeremysRose, JeremysItem],
				 [stella, StellasEvent, StellasRose, StellasItem],
				 [leroy, LeroysEvent, LeroysRose, LeroysItem]
				],

	member([jeremy, prom, _, _], Solutions),
	member([stella, _, cottage, _], Solutions),
	\+ member([stella, wedding, _, _], Solutions),

	member([hugh, _, pink, _], Solutions),
	\+ member([hugh, charity, _, _], Solutions),
	\+ member([hugh, wedding, _, _], Solutions),

	member([_, anniversary, _, streamers], Solutions),
	member([_, wedding, _, ballons], Solutions),

	member([_, _, sweet, chocolates], Solutions),
	\+ member([jeremy, _, mountain, _], Solutions),

	member([leroy, retirement, _, _], Solutions),
	member([_, prom, _, candles], Solutions),

	tell(hugh, HughsEvent, HughsRose, HughsItem),
	tell(ida, IdasEvent, IdasRose, IdasItem),
	tell(jeremy, JeremysEvent, JeremysRose, JeremysItem),
	tell(leroy, LeroysEvent, LeroysRose, LeroysItem),
	tell(stella, StellasEvent, StellasRose, StellasItem).
	
% Succeeds if all elements of the argument list are bound and different.
% Fails if any elements are unbound or equal to some other element.
all_different([H | T]) :- member(H, T), !, fail.
all_different([_ | T]) :- all_different(T).
all_different([_]).

tell(X, Y, Z, A) :-
	write(X), write(' is buying for a '),
	write(Y), write(' and bought '), 
	write(Z), write(' roses and '),
	write(A), write('.'), nl.