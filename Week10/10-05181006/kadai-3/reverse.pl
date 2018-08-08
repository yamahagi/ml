/* app.pl */
append([], Y, Y).
append([A|X], Y, [A|Z]) :-append(X, Y, Z).

/* reverse.pl */
reverse([],[]).
reverse([A|X],Y):-reverse(X,Z), append(Z, [A], Y).

/* concat.pl */
concat([],[]).
concat([A|X],Y):-concat(X,Z), append(A,Z, Y).
