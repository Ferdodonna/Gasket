:- module(gasket_math, [radius/2, umschreibender_kreis/4, naechste_loesung/5, initiale_kreise/6, skaliere_kreis/7, rotiere_kreis/4, spiegele_kreis/3]).

:- use_module(library(sort)).

% Gibt Radius von Kreis zurück
radius(kreis(_,Radius), Radius).

% Gibt den absoluten Radius eines generierten Kreises zurück
absoluter_radius(kreis(_,Radius)-_, AbsoluterRadius) :- AbsoluterRadius is abs(Radius).
absoluter_radius(kreis(_,Radius),AbsoluterRadius) :- AbsoluterRadius is abs(Radius).

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

% sortiere liste von kreisen nach radius
sortiere_anhand_radius([H|Liste],Sortiert) :-
	partition(groesser_radius(H),Liste,Groesser,Kleiner)
	, sortiere_anhand_radius(Groesser,GroesserSortiert)
	, sortiere_anhand_radius(Kleiner,KleinerSortiert)
	, append(GroesserSortiert,[H|KleinerSortiert],Sortiert)
.
sortiere_anhand_radius([],[]).

partition(_,[],[],[]).
partition(Pred,[H|T],[H|Erfuellt],ErfuelltNicht) :-
	call(Pred,H)
	, partition(Pred,T,Erfuellt,ErfuelltNicht)
.
partition(Pred,[H|T],Erfuellt,[H|ErfuelltNicht]) :-
	\+ call(Pred,H)
	, partition(Pred,T,Erfuellt,ErfuelltNicht)
.

groesser_radius(Kreis1,Kreis2) :-
	absoluter_radius(Kreis1,Radius1)
	, absoluter_radius(Kreis2,Radius2)
	, Radius2 >= Radius1
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

% Spiegelt einen Kreis an einem anderen Kreis
spiegele_kreis(kreis(KreisX/KreisY, Radius), kreis(SpiegelX/SpiegelY, SpiegelRadius), kreis(ErgebnisX/ErgebnisY, ErgebnisRadius)) :-
	DeltaX is KreisX - SpiegelX
	, DeltaY is KreisY - SpiegelY
	, Winkel is atan2(DeltaX, DeltaY)
	, Distanz is sqrt(DeltaX*DeltaX+DeltaY*DeltaY)
	, DistanzWeit is Distanz + abs(Radius)
	, DistanzNah is Distanz - abs(Radius)
	, SpiegelKonstante is SpiegelRadius * SpiegelRadius / 2
	, ErgebnisRadius is abs( (SpiegelKonstante / DistanzNah) - (SpiegelKonstante / DistanzWeit) )
	, DistanzNeu is (SpiegelKonstante / DistanzNah) + (SpiegelKonstante / DistanzWeit)
	, ErgebnisX is cos(Winkel) * DistanzNeu + SpiegelX
	, ErgebnisY is sin(Winkel) * DistanzNeu + SpiegelY
.

% Berechnet mit drei Radien die ersten drei Kreise
initiale_kreise(Radius1, Radius2, Radius3, kreis(-Radius1/0,Radius1), kreis(Radius2/0,Radius2), kreis(Kreis3X/Kreis3Y,Radius3)) :-
	Kreis3XNormalized is (Radius1*2*Radius1 + Radius1*2*Radius2 - Radius2*2*Radius3 + Radius3*2*Radius1) / 2 / (Radius1+Radius2)
	, Kreis3Y is sqrt( (Radius1 + Radius3) * (Radius1 + Radius3) - Kreis3XNormalized * Kreis3XNormalized)
	, Kreis3X is Kreis3XNormalized - Radius1
.

% new circles from touch points of old ones

touchpoint(kreis(X1/Y1,R1),kreis(X2/Y2,R2),Xt/Yt) :-
	Xt is X2 + (X1 - X2) * R2 / (R1 + R2)
	, Yt is Y2 + (Y1 - Y2) * R2 / (R1 + R2)
.

circle_from_three_points(P1,P2,P3,kreis(Kp,Kr)) :-
  line(P1,P2,P12,G12)
  , line(P2,P3,P23,G23)
  , !,cross(g(P12,-1/G12),g(P23,-1/G23),Kp)
  , distance(Kp,P1,Kr)
.

distance(X1/Y1,X2/Y2,D) :- D is sqrt((X1 - X2)^2 + (Y1 - Y2)^2).

line(X/Y,A/B,Xn/Yn,G) :-
  Dx is (X - A) / 2
  , Dy is (Y - B) / 2
  , Xn is A + Dx
  , Yn is B + Dy
  ,(
  Dx \= 0
  , Ga is Dy / Dx + 1
  , G is Ga - 1
  ;
  G = vertical
  )
.

cross(g(_,-1/vertical),g(_,-1/vertical),_) :- error_msg(invalid_points).
cross(g(X/_,-1/0.0),g(_/Y,-1/vertical),X/Y).
cross(g(_/Y,-1/vertical),g(X/_,-1/0.0),X/Y).
cross(g(_/Y1,-1/vertical),g(X2/Y2,G2),X/Y1) :- X is (Y1 - Y2 + X2 * G2) / G2.
cross(g(X1/Y1,G1),g(_/Y2,-1/vertical),X/Y2) :- X is (Y2 - Y1 + X1 * G1) / G1.
cross(g(_,-1/0.0),g(_,-1/0.0),_) :- error_msg(invalid_points).
cross(g(X1/_,-1/0.0),g(X2/Y2,G2),X1/Y) :- Y is X1 * G2 + Y2 - G2 * X2.
cross(g(X1/Y1,G1),g(X2/_,-1/0.0),X2/Y) :- Y is X2 * G1 + Y1 - G1 * X1.
cross(g(X1/Y1,G1),g(X2/Y2,G2),X/Y) :-
  Dg is G2 - G1
  , (
  Dg = 0.0, !,error_msg(invalid_points)
  ;
  X is (Y1 - Y2 + G2*X2 - G1*X1) / (G2 - G1)
  , Y is G1 * X + Y1 - G1 * X1
  )
.

error_msg(Info) :-
  message(Info,Str)
  , write(Str)
  , fail
.
message(invalid_points,"The three given points can not form a circle!").
