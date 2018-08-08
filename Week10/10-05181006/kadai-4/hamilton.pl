/*hamilton*/

/* app.pl */
append([], Y, Y).
append([A|X], Y, [A|Z]) :-append(X, Y, Z).


hamilton_rec([],_,_).
hamilton_rec(V,E,U) :- append(_,[[U|Q]|_],E),append(V1, [Q|V2], V), append(V1, V2, VN),hamilton_rec(VN, E, Q).
hamilton(V, E) :- append(V1, [U|V2], V), append(V1, V2, VN), hamilton_rec(VN, E, U).
