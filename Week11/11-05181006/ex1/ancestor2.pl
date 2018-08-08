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

ancestor(X,Y) :-parent(X,Y).
ancestor(X,Y) :-ancestor(Z,Y), parent(X,Z).
