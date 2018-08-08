finish(P, [P,P,P,_,_,_,_,_,_]).
finish(P, [_,_,_,P,P,P,_,_,_]).
finish(P, [_,_,_,_,_,_,P,P,P]).
finish(P, [P,_,_,P,_,_,P,_,_]).
finish(P, [_,P,_,_,P,_,_,P,_]).
finish(P, [_,_,P,_,_,P,_,_,P]).
finish(P, [P,_,_,_,P,_,_,_,P]).
finish(P, [_,_,P,_,P,_,P,_,_]).

next(P, [e,A,B,C,D,E,F,G,H], [P,A,B,C,D,E,F,G,H]).
next(P, [A,e,B,C,D,E,F,G,H], [A,P,B,C,D,E,F,G,H]).
next(P, [A,B,e,C,D,E,F,G,H], [A,B,P,C,D,E,F,G,H]).
next(P, [A,B,C,e,D,E,F,G,H], [A,B,C,P,D,E,F,G,H]).
next(P, [A,B,C,D,e,E,F,G,H], [A,B,C,D,P,E,F,G,H]).
next(P, [A,B,C,D,E,e,F,G,H], [A,B,C,D,E,P,F,G,H]).
next(P, [A,B,C,D,E,F,e,G,H], [A,B,C,D,E,F,P,G,H]).
next(P, [A,B,C,D,E,F,G,e,H], [A,B,C,D,E,F,G,P,H]).
next(P, [A,B,C,D,E,F,G,H,e], [A,B,C,D,E,F,G,H,P]).

aite(p,q).
aite(q,p).

full(B) :- next(p,B,_),!,false.
full(_).

win(P,B) :- aite(P,_),finish(P,B).
win(P,B) :- aite(P,Q),\+ finish(Q,B),next(P,B,C),lose(Q,C).
lose(P,B) :- aite(P,Q),finish(Q,B).
/*終局ではなく、そしてlose2の否定 つまりBから全ての一手進めた盤面Cに対してwin(Q,C)がtrueの時true*/
lose(P,B) :- aite(P,_),\+full(B),\+ lose_sub(P,B).
/*ある一手進めた盤面Cに対してwin(Q,C)がfalseになる時trueとなる。
つまり、全ての盤面Cに対してwin(Q,C)がtrueとなるならfalseとなる*/
lose_sub(P,B) :- aite(P,Q),next(P,B,C),\+win(Q,C).
