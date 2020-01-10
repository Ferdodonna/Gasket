% k4 = k1 + k2 + k3 +- 2 * sqrt(k1k2+k2k3+k3k1)
:- dynamic (done/3),(counter/1)).
counter(0).

kehr(1/B,B).
kehr(B,1/B).

radii(R1,R2,R3,Ri,Ro):-
  maplist(kehr,[R1,R2,R3],[K1,K2,K3]),
  Ksum = K1 + K2 + K3,
  Kroot = sqrt(K1*K2 + K2*K3 + K3*K1),
  Ki = Ksum - 2 * Kroot,
  Ko = Ksum + 2 * Kroot,
  (
  Ki < 0 ->
    kehr(-Ki,Ro),
    kehr(Ko,Ri)
    ;
    kehr(Ki,Ri),
    kehr(-Ko,Ro)
  ).

% herons formula:
% Area = sqrt(p(p-a)(p-b)(p-c))
% p = (a+b+c) / 2
area(A,B,C,Area) :-
    P = (A+B+C) / 2,
    Area = sqrt(P*(P-A)*(P-B)*(P-C)).

circumface(B,C,A,U) :-
  U = A + B + C.


dist(kart(X1,Y1),kart(X2,Y2),D) :-
  D = sqrt((X1 - X2)^2 + (Y1-Y2)^2).

choose(0,_,[]).
choose(N,[A|B],[A|R]) :-
  Nl is N -1,
  choose(Nl,B,R).
choose(N,[_|B],R) :- choose(N,B,R).

isopP(triangle(P1,P2,P3),IP) :-
  dist(P1,P2,A),
  dist(P2,P3,B),
  dist(P3,P1,C),
  area(A,B,C,Area),
  circumface(A,B,C,U),
  S = U / 2,
  maplist(baryfy(S,Area),[A,B,C],[IPA,IPB,IPC]),
  IP = bary(IPA,IPB,IPC).

baryfy(S,Area,A,Bary):-
  Bary = A - Area / (S - A).

fromBary(triangle(kart(X1,Y1),
                  kart(X2,Y2),
                  kart(X3,Y3)),
         bary(A,B,C),
         kart(X,Y))
         :-
  Bsum = A+B+C
  X = (A * X1 + B * X2 + C * X3) / Bsum,
  Y = (A * Y1 + B * Y2 + C * Y3) / Bsum.

/*

Tagge einzelne Kreise mit Nummern, da wir die Radien und so weiter ja noch nicht ausrechnen.
Asserte bereits verwendete Kreistripel als done(a,b,c) a<b<c
Habe counter zum produzieren der Tag-Nummern as dynamic predicate

*/
