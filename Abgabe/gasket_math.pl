:- module(gasket_math, [radius/2, umschreibender_kreis/4, naechste_loesung/5, initiale_kreise/6, skaliere_kreis/7, rotiere_kreis/4]).

% Gibt Radius von Kreis zurück
radius(kreis(_,Radius), Radius).

% Gibt Position von Kreis zurück
position(kreis(Position,_),Position).

% Konvertiert Baryzentrische Koordinaten zu kartesischen
baryzentrisch2kartesisch(
	A/B/C
	, dreieck(X1/Y1, X2/Y2, X3/Y3)
	, X/Y
) :-
	U is A + B + C
	, X is (A * X3 + B * X1 + C * X2) / U
	, Y is (A * Y3 + B * Y1 + C * Y2) / U
.

% Transformiert einen Kreis anhand eines Skalierung und eines Offsets (X/Y)
skaliere_kreis(kreis(KreisX/KreisY,KreisRadius), OffsetXPre, OffsetYPre, Skalierung, OffsetX, OffsetY, kreis(KreisNeuX/KreisNeuY, KreisNeuRadius)) :-
	KreisNeuX is ( KreisX - OffsetXPre ) * Skalierung + OffsetX
	, KreisNeuY is ( KreisY - OffsetYPre ) * Skalierung + OffsetY
	, KreisNeuRadius is KreisRadius * Skalierung
.

% Rotiert einen Kreis um das Zentrum eines anderen
rotiere_kreis(kreis(KreisX/KreisY,KreisRadius), kreis(RotationX/RotationY, _), Rotation, kreis(ErgebnisX/ErgebnisY, KreisRadius)) :-
	DeltaX is KreisX - RotationX
	, DeltaY is KreisY - RotationY
	, Winkel is atan2(DeltaX, DeltaY) + Rotation
	, Distanz is sqrt(DeltaX*DeltaX+DeltaY*DeltaY)
	, ErgebnisX is cos(Winkel) * Distanz + RotationX
	, ErgebnisY is sin(Winkel) * Distanz + RotationY
.

% Berechnet zu drei Kreisen den Umschreibenden Kreis
umschreibender_kreis(kreis(P1,R1),kreis(P2,R2),kreis(P3,R3),kreis(UmschreibendPosition, UmschreibendRadius)) :-

	% Position Berechnung
	A is R1 + R2
	, B is R2 + R3
	, C is R3 + R1
	, Uh is R1 + R2 + R3 % Halber Umfang
	, I is sqrt(Uh * (Uh - A) * (Uh - B) * (Uh - C)) % Flächeninhalt
	, BaryA is A - I / (Uh - A)
	, BaryB is B - I / (Uh - B)
	, BaryC is C - I / (Uh - C)

	% Radius Berechnung
	, InvSum is 1/R1 + 1/R2 + 1/R3
	, InvRoot is sqrt( 1 / (R1 * R2) + 1 / (R2 * R3) + 1 / (R1 * R3) )
	, UmschreibendRadius is 1 / ( InvSum - 2 * InvRoot )

	% Konvertierung
	, baryzentrisch2kartesisch(BaryA/BaryB/BaryC, dreieck(P1,P2,P3), UmschreibendPosition)
.

% Gibt inverses Element relative zur Multiplikation zurück
kehr(V,1/V).

% Berechnet die zweite Lösung (den zweiten zu drei paarweise tangierenden Kreisen tangierenden Kreis) mithilfe der ersten Lösung
% Vietas Theorem: Bei einer Gleichung mit zwei Lösungen ist die zweite Lösung einfacher deduzierbar, wenn man die Erste bereits hat
naechste_loesung(
	Kreis1
	, Kreis2
	, Kreis3
	, VorherigeLoesung
	, kreis(NaechsteLoesungX/NaechsteLoesungY,NaechsteLoesungRadius)
)
:-
	% Radius berechnen
	maplist(radius,[VorherigeLoesung,Kreis1,Kreis2,Kreis3],Radien)
	, maplist(kehr,Radien,[LE,L1,L2,L3])
	, LZ is 2 * (L1 + L2 + L3) - LE
	, NaechsteLoesungRadius is 1/LZ

	% Position berechnen
	, maplist(position,[VorherigeLoesung,Kreis1,Kreis2,Kreis3],[Xe/Ye,X1/Y1,X2/Y2,X3/Y3])
	, NaechsteLoesungX is (2 * (L1*X1 + L2*X2 + L3*X3) - LE*Xe) / LZ
	, NaechsteLoesungY is (2 * (L1*Y1 + L2*Y2 + L3*Y3) - LE*Ye) / LZ
.

% Berechnet mit drei Radien die ersten drei Kreise
initiale_kreise(Radius1, Radius2, Radius3, kreis(-Radius1/0,Radius1), kreis(Radius2/0,Radius2), kreis(Kreis3X/Kreis3Y,Radius3)) :-
	Kreis3XNormalized is (Radius1*2*Radius1 + Radius1*2*Radius2 - Radius2*2*Radius3 + Radius3*2*Radius1) / 2 / (Radius1+Radius2)
	, Kreis3Y is sqrt( (Radius1 + Radius3) * (Radius1 + Radius3) - Kreis3XNormalized * Kreis3XNormalized)
	, Kreis3X is Kreis3XNormalized - Radius1
.
