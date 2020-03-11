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


% Below, you will define several Prolog predicates that describe family
% relationships. Your building blocks will be the four basic predicates defined
% above: `female/1`, `male/1`, `married/2`, and `parent/2`.
% 
% For each predicate, I have provided some example queries and the unifiers
% (solutions) that should be produced, or `false` if no unifier exists. It is not
% important what order the unifiers are produced in, and it is OK if a unifier is
% produced more than once. However, for all queries, *all valid unifiers* should
% eventually be produced, *no invalid unifiers* should be produced, and the query
% should not induce an infinite loop.


% 1. Define a predicate `child/2` that inverts the parent relationship.
%    
%    ?- child(marge,X).
%    X = clancy ;
%    X = jackie .
%    
%    ?- child(X,marge).
%    X = bart ;
%    X = lisa ;
%    X = maggie .


% 2. Define two predicates `isMother/1` and `isFather/1`.
%    
%    ?- isMother(selma).
%    true .
%    
%    ?- isMother(patty).
%    false .
%    
%    ?- isFather(X).
%    X = abe ;
%    X = clancy ;
%    X = homer .


% 3. Define a predicate `grandparent/2`.
%    
%    ?- grandparent(abe,X).
%    X = bart ;
%    X = lisa ;
%    X = maggie .
%    
%    ?- grandparent(X,ling).
%    X = clancy ;
%    X = jackie .


% 4. Define a predicate `sibling/2`. Siblings share at least one parent.
%    
%    ?- sibling(homer,X).
%    X = herb .
%    
%    ?- sibling(X,lisa).
%    X = bart ;
%    X = maggie .


% 5. Define two predicates `brother/2` and `sister/2`.
%
%    ?- sister(lisa,X).
%    X = bart ;
%    X = maggie .
%    
%    ?- sister(X,lisa).
%    X = maggie .
%    
%    ?- brother(bart,X).
%    X = lisa ;
%    X = maggie .
%    
%    ?- brother(X,bart).
%    false.


% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%    a sibling or the sibling of a spouse.
%    
%    ?- siblingInLaw(selma,X).
%    X = homer .
%    
%    ?- siblingInLaw(marge,X).
%    X = herb .
%    
%    ?- siblingInLaw(X,homer).
%    X = patty ;
%    X = selma .


% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.
%    
%    ?- aunt(patty,X).
%    X = bart ;
%    X = lisa ;
%    X = maggie ;
%    X = ling .
%    
%    ?- uncle(X,ling).
%    X = homer .


% 8. Define the predicate `cousin/2`.
%    
%    ?- cousin(maggie,X).
%    X = ling .
%    
%    ?- cousin(X,ling).
%    X = bart ;
%    X = lisa ;
%    X = maggie .


% 9. Define the predicate `ancestor/2`.
%    
%    ?- ancestor(abe,X).
%    X = herb ;
%    X = homer ;
%    X = bart ;
%    X = lisa ;
%    X = maggie .
%    
%    ?- ancestor(X,lisa).
%    X = homer ;
%    X = marge ;
%    X = abe ;
%    X = mona ;
%    X = clancy ;
%    X = jackie .


