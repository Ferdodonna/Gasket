radius(kreis(_,R), R).

% Berechnet Flächeninhalt eines Dreiecks
inhalt(dreieck(A,B,C),I) :- P = (A + B + C) / 2, I is sqrt(P * (P - A) * (P - B) * (P - C)).

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
	seiten(dreieck(P1,P2,P3),dreieck(A,B,C))
	, inhalt(dreieck(A,B,C),I)
	, umfang(dreieck(A,B,C),U)
	, UH is U / 2
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
	seiten(dreieck(P1,P2,P3),dreieck(A,B,C))
	, inhalt(dreieck(A,B,C),I)
	, umfang(dreieck(A,B,C),U)
	, UH is U / 2
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


% Berechnet zu zwei inneren Kreisen und einem umschreibenden Kreis den inbeschriebenen Kreis
tangentialer_kreis(kreis(P1,R1),kreis(P2,R2),/* umschreibender Kreis */ kreis(P3,R3),KT) :-

	% Position Berechnung
	seiten(dreieck(P1,P2,P3),dreieck(A,B,C))
	, inhalt(dreieck(A,B,C),I)
	, umfang(dreieck(A,B,C),U)
	, UH is U / 2
	, BaryA is A + I / (UH - A)
	, BaryB is B + I / (UH - B)
	, BaryC is C + I / (UH - C)
	
	% Radius Berechnung
	, K1 is 1 / R1
	, K2 is 1 / R2
	, K3 is 1 / R3
	, RT is 1 / ( -2 * sqrt(K1 * K2 - K1 * K3 - K2 * K3) + K1 + K2 - K3 )
	
	% Konvertierung
	, baryzentrisch2kartesisch(baryzentrisch(BaryA,BaryB,BaryC), dreieck(P1,P2,P3), PT)
	
	, KT = kreis(PT, RT)
.


% Konvertiert Baryzentrische Koordinaten zu kartesischen
baryzentrisch2kartesisch(
	baryzentrisch(A,B,C)
	, dreieck(kartesisch(X1,Y1), kartesisch(X2,Y2), kartesisch(X3,Y3))
    , kartesisch(X,Y)
) :-
	Bsum is A + B + C
	, X is (A * X3 + B * X1 + C * X2) / Bsum
	, Y is (A * Y3 + B * Y1 + C * Y2) / Bsum
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
	, write("\n")
	, umschreibender_kreis(K1,K2,K3,KU)
	, write(KU)
	, write("\n")
	, tangentialer_kreis(K3,K2,KU,KI)
	, write(KI)
.
