male(kobo).
male(koji).
male(iwao).
female(sanae).
female(mine).
female(miho).

parent(kobo, koji).
parent(kobo, sanae).
parent(miho, koji).
parent(miho, sanae).
parent(sanae, iwao).
parent(sanae, mine).

sibling(X,Y):- parent(X,Z),parent(Y,Z).

ancestor(X,Z) :-parent(X,Z).
ancestor(X,Z) :-parent(X,Y), ancestor(Y,Z).

bloodrelative(X,Y) :- ancestor(X,Z),ancestor(Y,Z);ancestor(X,Y);ancestor(Y.X).
