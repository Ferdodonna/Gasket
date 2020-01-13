% TODO:
/*
- vorraussetzen dass zwei der tangierenden punkte unterschiedlich sind


*/
radius(kreis(_,R), R).
kehr(R,T) :- T is 1/R.

% Berechnet Flächeninhalt eines Dreiecks
inhalt(dreieck(A,B,C),P,I) :- I is sqrt(P * (P - A) * (P - B) * (P - C)).

% Berechnet Umfang eines Dreiecks
umfang(dreieck(A,B,C),U) :- U is A + B + C.

% Gibt Position zurück
position(kreis(P,_),P).
position(kartesisch(X,Y),kartesisch(X,Y)).

% Berechnet Abstand zweier Objekte mit Position
abstand(kartesisch(X1,Y1),kartesisch(X2,Y2),D) :- D is sqrt((X1 - X2)^2 + (Y1-Y2)^2).


choose(0,A,[],A).
choose(N,[A|B],[A|R],L) :-
  Nl is N -1,
  choose(Nl,B,R,L).
choose(N,[A|B],R,[A|L]) :- choose(N,B,R,L).

% Kreiert ein Dreieck aus drei Positionen
seiten(dreieck(P1,P2,P3),dreieck(A,B,C)) :-
	abstand(P1,P2,A)
	, abstand(P2,P3,B)
	, abstand(P3,P1,C)
.

% Berechnet zu drei Kreisen den Umschreibenden Kreis
umschreibender_kreis(kreis(P1,R1),kreis(P2,R2),kreis(P3,R3),KU) :-

	% Position Berechnung
  A is R1 + R2
  , B is R2 + R3
  , C is R3 + R1
	, umfang(dreieck(A,B,C),U)
	, UH is U / 2
	, inhalt(dreieck(A,B,C),UH,I)
	, BaryA is A - I / (UH - A)
	, BaryB is B - I / (UH - B)
	, BaryC is C - I / (UH - C)

	% Radius Berechnung
	, InvSum is 1/R1 + 1/R2 + 1/R3
	, InvRoot is sqrt( 1 / (R1 * R2) + 1 / (R2 * R3) + 1 / (R1 * R3) )
	, RU is 1 / ( InvSum - 2 * InvRoot )

	% Konvertierung
	, baryzentrisch2kartesisch(baryzentrisch(BaryA,BaryB,BaryC), dreieck(P1,P2,P3), PU)

	, KU = kreis(PU, RU)
.

% Berechnet zu drei Kreisen den Umschreibenden Kreis
inbeschriebener_kreis(kreis(P1,R1),kreis(P2,R2),kreis(P3,R3),KI) :-

	% Position Berechnung
	A is R1 + R2
  , B is R2 + R3
  , C is R3 + R1
	, umfang(dreieck(A,B,C),U)
	, UH is U / 2
	, inhalt(dreieck(A,B,C),UH,I)
	, BaryA is A + I / (UH - A)
	, BaryB is B + I / (UH - B)
	, BaryC is C + I / (UH - C)

	% Radius Berechnung
	, InvSum is 1/R1 + 1/R2 + 1/R3
	, InvRoot is sqrt( 1 / (R1 * R2) + 1 / (R2 * R3) + 1 / (R1 * R3) )
	, RU is 1 / ( InvSum + 2 * InvRoot )

	% Konvertierung
	, baryzentrisch2kartesisch(baryzentrisch(BaryA,BaryB,BaryC), dreieck(P1,P2,P3), PU)

	, KI = kreis(PU, RU)
.


kartesisch2baryzentrisch(
	kartesisch(X,Y)
	, dreieck(kartesisch(X1,Y1), kartesisch(X2,Y2), kartesisch(X3,Y3))
	, baryzentrisch(A,B,C)
) :-
	A is (Y3 - Y2) * X + (X2 - X3) * Y + (X3 * Y2 - X2 * Y3)
	, B is (Y1 - Y3) * X + (X3 - X1) * Y + (X1 * Y3 - X3 * Y1)
	, C is (Y2 - Y1) * X + (X1 - X2) * Y + (X2 * Y1 - X1 * Y2)
.

% Kreis 3 umschreibt Kreis 1 und 2
einer_aussen_zwei_innen(/*innere Kreise*/kreis(P1,R1),kreis(P2,R2),/*äußerer Kreis*/ kreis(P3,R3),K4,K5) :-
  % Radii
  R3 < 0 % ist ja außen
  , K1 is 1/R1
  , K2 is 1/R2
  , K3 is 1/R3
  , Ksum is K1 + K2 + K3
  , Ksqrt is sqrt(K1 * K2 + K2 * K3 + K3 * K1)
  , R4 is 1 / (Ksum + 2 * Ksqrt)
  , R5 is 1 / (Ksum - 2 * Ksqrt)

  % Center
  % 4
  , A1 is R1 + R2
  , B1 is R2 + R4
  , C1 is R4 + R1
  , umfang(dreieck(A1,B1,C1),U1)
  , UH1 is U1 / 2
  , inhalt(dreieck(A1,B1,C1),UH1,I1)
  , BaryA1 is A1 - I1 / (UH1 - A1)
  , BaryB1 is B1 - I1 / (UH1 - B1)
  , BaryC1 is C1 - I1 / (UH1 - C1)

  , baryzentrisch2kartesisch(baryzentrisch(BaryA1,BaryB1,BaryC1)
                            , dreieck(P1,P2,P4)
                            , P3)
  , K4 = kreis(P4,R4)

  % 5
  , A2 is R1 + R2
  , B2 is R2 + R5
  , C2 is R5 + R1
  , umfang(dreieck(A2,B2,C2),U2)
  , UH2 is U2 / 2
  , inhalt(dreieck(A2,B2,C2),UH2,I2)
  , BaryA2 is A2 - I2 / (UH2 - A2)
  , BaryB2 is B2 - I2 / (UH2 - B2)
  , BaryC2 is C2 - I2 / (UH2 - C2)

  , baryzentrisch2kartesisch(baryzentrisch(BaryA2,BaryB2,BaryC2)
                            , dreieck(P1,P2,P5)
                            , P3)
  , K5 = kreis(P5,R5)
.

% Konvertiert Baryzentrische Koordinaten zu kartesischen
baryzentrisch2kartesisch(
	baryzentrisch(A,B,C)
	, dreieck(kartesisch(X1,Y1), kartesisch(X2,Y2), kartesisch(X3,Y3))
  , kartesisch(X,Y)
) :-
  maplist(nonvar,[A,B,C,X1,Y1,X2,Y2,X3,Y3])
  , !
	, Bsum is A + B + C
	, X is (A * X3 + B * X1 + C * X2) / Bsum
	, Y is (A * Y3 + B * Y1 + C * Y2) / Bsum
.
baryzentrisch2kartesisch(
	baryzentrisch(A,B,C)
	, dreieck(kartesisch(X1,Y1), kartesisch(X2,Y2), kartesisch(X3,Y3))
  , kartesisch(X,Y)
) :-
  maplist(nonvar,[A,B,C,X1,Y2,X2,Y2,X,Y])
  , !
  , Bsum is A + B + C
  , X3 is (X * Bsum - B * X1 - C * X2) / A
  , Y3 is (Y * Bsum - B * Y1 - C * Y2) / A
.

% gegeben drei radii (radien?) gibt dir drei tangierende Kreise mit diesen Radien (radii? ;))
complete(R1,R2,R3,K1,K2,K3) :-
  P1 = kartesisch(0,0)
  , R12 is R1 + R2
  , P2 = kartesisch(R12,0)
  , X3 is (R1*R1 + R1*R3 + R1*R2 - R2*R3) / (R1+R2)
  , Y3 is sqrt((R1+R3) * (R1+R3) - X3*X3)
  , P3 = kartesisch(X3,Y3)
  , maplist(=,[K1,K2,K3],[kreis(P1,R1),kreis(P2,R2),kreis(P3,R3)])
.


:- discontiguous compute/4.
compute(1,K1,K2,K3) :-
	K1 = kreis(kartesisch(100, 100),100)
	, K2 = kreis(kartesisch(300,100),100)
	, P3 = kartesisch(200, 300)
	, position(K1,P1)
	, abstand(P1,P3,D)
	, radius(K1, R1)
	, R3 is D-R1
	, K3 = kreis(P3,R3)
	, write(K3)
	, nl
	, umschreibender_kreis(K1,K2,K3,KU)
	, write(KU)
  , inbeschriebener_kreis(K1,K2,K3,KI)
  , nl
  , write(KI)
  , einer_aussen_zwei_innen(K1,K2,KU,K3N,K4)
  , nl
  , write(K3N)
  , nl
  , write(K4)
.

% Die Formel aus einer_aussen_zwei_innen/5 funktioniert für das hier nicht
% warum auch immer
% Konstellation mit einem äußeren Kreis und zwei inneren Kreisen, wobei die inneren Kreise auf der
% Kreishalbierenden des äußeren Kreises liegen
print(X) :- write(X),nl.
compute(2,K1,K2,K3) :-
  C1=kartesisch(400,400)
  , R1=100
  , C2 = kartesisch(600,600)
  , abstand(C1,C2,A)
  , R2 is A + R1
  , R3 is R2 - R1
  , gradient(C1,C2,G12)
  , MD is R1 + R3
  , move(MD,gerade(C1,G12),C3)
  , K1 = kreis(C1,R1)
  , K2 = kreis(C2,-R2) % ist ja der große Kreis
  , K3 = kreis(C3,R3)
  , maplist(print,[K1,K2,K3])
  , einer_aussen_zwei_innen(K1,K3,K2,KG,KT)
  , maplist(print,[KG,KT])
.

% !!!!!! BAUSTELLE: BETRETEN AUF EIGENE GEFAHR !!!!!!

% Predikat ist wahr wenn PC und PD auf unterschiedlichen Seiten der Gerade durch
% P1 und P2 sind
gegenseitig(P1,P2,PC,PD) :-
  gradient(P1,P2,G)
  , orthogonal(G,Go)
  , vektorabstand_punkt_gerade(gerade(PC,Go),gerade(P1,G),kartesisch(CX,CY))
  , vektorabstand_punkt_gerade(gerade(PD,Go),gerade(P1,G),kartesisch(DX,DY))
  , 0 < DX * CX * DY * CY
.

% Minimaler Vektor zwischen einem Punkt P und einer Gerade Gerade2 oder so, ich
% gebe den Gradient der kürzesten Strecke von P zu Gerade2 mit weil ichs kann
vektorabstand_punkt_gerade(gerade(P,G),Gerade2,VAbstand) :-
  schnittpunkt(gerade(P,G),Gerade2,SP)
  , abstand_vektor(P,SP,VAbstand)
.

% Vektor zwischen zwei Punkten
abstand_vektor(kartesisch(X1,Y1),kartesisch(X2,Y2),kartesisch(X,Y)) :-
  X is X1 - X2
  , Y is Y1 - Y2
.

% Gradient einer Gerade definiert durch zwei Punkte
gradient(kartesisch(A,B),kartesisch(C,D),G) :-
  A \= C
  , G is (B - D) / (A - C)
.

% Predikat hält wenn die beiden Geraden/Gradienten orthogonal sind
orthogonal(gerade(_,G1),gerade(_,G2)) :-
  !,orthogonal(G1,G2)
.
orthogonal(G1,G2) :-
  nonvar(G2)
  , G2 \= 0
  , G1 is (-1) * (1 / G2)
.
orthogonal(G1,G2) :-
  nonvar(G1)
  , G1 \= 0
  , G2 is (-1) * (1 /G1)
.

% Schnittpunkt zweier Geraden
schnittpunkt(gerade(kartesisch(X1,Y1),G1),gerade(kartesisch(X2,Y2),G2),kartesisch(X,Y)) :-
  G1 \= G2
  , YZ1 is Y1 - X1 * G1
  , YZ2 is Y2 - X2 * G2
  , X is (YZ2 - YZ1) / (G1- G2)
  , Y is X*G1 + YZ1
.

% bewege Punkt X,Y mit dem Gradienten die Distanz D und bekomme Rx,Ry
move(D,gerade(kartesisch(X,Y),G),kartesisch(Rx,Ry)) :-
  Tx is sqrt((D * D) / (1 + G * G)),
  Ty is G * Tx,
  (D < 0 ->
    Rx is X - Tx,
    Ry is Y - Ty
  ;
    Rx is X + Tx,
    Ry is Y + Ty).


% Habe hier den anderen Weg versucht, klappt aber nicht:
% habe die Formeln aus dem Kopf hingeschrieben und wahrscheinlich etwas falsch gemacht
% ne hab nichts falsch gemacht, klappt einfach nicht.
scalarProd(S,kartesisch(X,Y),kartesisch(SX,SY)) :-
  maplist(is,[SX,SY],[S*X,S*Y]).

findCenters(kreis(P1,R1),kreis(P2,R2),kreis(P3,R3),kreis(P4,R4),kreis(P5,R5)) :-
  maplist(kehr,[R1,R2,R3],[K1,K2,K3])
  , descartes(K1,K2,K3,K4,K5)
  , maplist(scalarProd,[K1,K2,K3],[P1,P2,P3],[kartesisch(ZX1,ZY1),kartesisch(ZX2,ZY2),kartesisch(ZX3,ZY3)])
  , descartes(ZX1,ZX2,ZX3,ZX4,ZX5)
  , descartes(ZY1,ZY2,ZY3,ZY4,ZY5)
  , X4 is ZX4 / K4, Y4 is ZY4 / K4
  , X5 is ZX5 / K5, Y5 is ZY5 / K5
  , P4 = kartesisch(X4,Y4)
  , P5 = kartesisch(X5,Y5)
  , R4 is 1 / K4
  , R5 is 1 / K5
.

descartes(K1,K2,K3,K4,K5) :-
  maplist(is,[Ksum    , Ksqrt                  , K4          , K5          ]
            ,[K1+K2+K3, sqrt(K1*K2+K2*K3+K3*K1), Ksum+2*Ksqrt, Ksum-2*Ksqrt]).


% berechnet zweiten tangierenden Kreis zu drei tangierenden Kreisen
% falls man den ersten bereits berechnet hat
zweiterKreis(Erster,K1,K2,K3,Zweiter) :-

  % Radius berechnen
  maplist(radius,[Erster,K1,K2,K3],Radien /*Radii ?*/)
  , maplist(kehr,Radien,[LE,L1,L2,L3])
  , LZ is (2 * (L1 + L2 + L3) - LE)
  , RZ is 1/LZ

  % Postiion berechnen
  , maplist(position,[Erster,K1,K2,K3],[kartesisch(Xe,Ye),kartesisch(X1,Y1),kartesisch(X2,Y2),kartesisch(X3,Y3)])
  , XZ is (2 * (L1*X1 + L2*X2 + L3*X3) - LE*Xe) / LZ
  , YZ is (2 * (L1*Y1 + L2*Y2 + L3*Y3) - LE*Ye) / LZ
  , Zweiter = kreis(kartesisch(XZ,YZ),RZ)
.
