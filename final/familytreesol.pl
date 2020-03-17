% problem description link: https://piazza.com/class/k525fgnk15s5vc?cid=255

% Here are a bunch of facts describing the Simpson's family tree.
% Don't change them!
female(mona).
female(jackie).
female(marge).
female(patty).
female(selma).
female(lisa).
female(maggie).
female(ling).

male(abe).
male(clancy).
male(herb).
male(homer).
male(bart).

married_(abe,mona).
married_(clancy,jackie).
married_(homer,marge).

married(X,Y) :- married_(X,Y).
married(X,Y) :- married_(Y,X).

parent(abe,herb).
parent(abe,homer).
parent(mona,homer).

parent(clancy,marge).
parent(jackie,marge).
parent(clancy,patty).
parent(jackie,patty).
parent(clancy,selma).
parent(jackie,selma).

parent(homer,bart).
parent(marge,bart).
parent(homer,lisa).
parent(marge,lisa).
parent(homer,maggie).
parent(marge,maggie).

parent(selma,ling).


% 1. Define a predicate `child/2` that inverts the parent relationship.
child(X,Y) :- parent(Y,X).

% 2. Define two predicates `isMother/1` and `isFather/1`.
isMother(X) :- parent(X,_), female(X).
isFather(X) :- parent(X,_), male(X).

% 3. Define a predicate `grandparent/2`.
grandparent(X,Y) :- parent(X,Z), parent(Z,Y).

% 4. Define a predicate `sibling/2`. Siblings share at least one parent.
sibling(X,Y) :- parent(Z,X), parent(Z,Y), X \= Y.

% 5. Define two predicates `brother/2` and `sister/2`.
brother(X,Y) :- male(X), sibling(X,Y).
sister(X,Y)  :- female(X), sibling(X,Y).

% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%    a sibling or the sibling of a spouse.
siblingInLaw(X,Y) :- married(X,Z), sibling(Z,Y).
siblingInLaw(X,Y) :- sibling(X,Z), married(Z,Y).

brotherInLaw(X,Y) :- male(X), siblingInLaw(X,Y).
sisterInLaw(X,Y)  :- female(X), siblingInLaw(X,Y).

% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.
aunt(X,Y) :- parent(Z,Y), sister(X,Z).
aunt(X,Y) :- parent(Z,Y), sisterInLaw(X,Z).
uncle(X,Y) :- parent(Z,Y), brother(X,Z).
uncle(X,Y) :- parent(Z,Y), brotherInLaw(X,Z).

% 8. Define the predicate `cousin/2`.
cousin(X,Y) :- parent(W,X), parent(Z,Y), sibling(W,Z).

% 9. Define the predicate `ancestor/2`.
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,Z), ancestor(Z,Y).
