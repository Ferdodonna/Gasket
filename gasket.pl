radius(kreis(_,R), R).

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
	, RU is abs( 1 / ( InvSum - 2 * InvRoot ) )

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
	, RU is abs( 1 / ( InvSum + 2 * InvRoot ) )

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

compute :-
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
  , KU = kreis(PU,RU)
  , KNU = kreis(PU,-RU)
  , einer_aussen_zwei_innen(K1,K2,KNU,K3N,K4)
  , nl
  , write(K3N)
  , nl
  , write(K4)
.

% Predikat ist wahr wenn PC und PD auf unterschiedlichen Seiten der Gerade durch
% P1 und P2 sind
gegenseitig(P1,P2,PC,PD) :-
  gradient(P1,P2,G)
  , orthogonal(G,Go)
  , vektorabstand_punkt_gerade(gerade(PC,Go),gerade(P1,G),kart(CX,CY))
  , vektorabstand_punkt_gerade(gerade(PD,Go),gerade(P1,G),kart(DX,DY))
  , F1 is DX * CX
  , F2 is DY * CY
  , 0 < F1 * F2
.

% Minimaler Vektor zwischen einem Punkt P und einer Gerade Gerade2 oder so, ich
% gebe den Gradient der kürzesten Strecke von P zu Gerade2 mit weil ichs kann
vektorabstand_punkt_gerade(gerade(P,G),Gerade2,VAbstand) :-
  schnittpunkt(gerade(P,G),Gerade2,SP)
  , abstand_vektor(P,SP,VAbstand)
.

% Vektor zwischen zwei Punkten
abstand_vektor(kart(X1,Y1),kart(X2,Y2),kart(X,Y)) :-
  X is X1 - X2
  , Y is Y1 - Y2
.

% Gradient einer Gerade definiert durch zwei Punkte
gradient(kart(A,B),kart(C,D),G) :-
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
schnittpunkt(gerade(kart(X1,Y1),G1),gerade(kart(X2,Y2),G2),kart(X,Y)) :-
  G1 \= G2
  , YZ1 is Y1 - X1 * G1
  , YZ2 is Y2 - X2 * G2
  , X is (YZ2 - YZ1) / (G1- G2)
  , Y is X*G1 + YZ1
.
