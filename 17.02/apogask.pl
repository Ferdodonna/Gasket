:- use_module(library(lists)).
:- use_module(library(clpfd)).

radius(kreis(_,R), R).
kehr(V,1/V).

% Berechnet Flächeninhalt eines Dreiecks
inhalt(dreieck(A,B,C),P,I) :- I is sqrt(P * (P - A) * (P - B) * (P - C)).

% Berechnet Umfang eines Dreiecks
umfang(dreieck(A,B,C),U) :- U is A + B + C.

% Gibt Position zurück
position(kreis(P,_),P).
position(X/Y,X/Y).

% Berechnet Abstand zweier Objekte mit Position
abstand(X1/Y1,X2/Y2,D) :- D is sqrt((X1 - X2)^2 + (Y1-Y2)^2).

% Berechnet zu drei Kreisen den Umschreibenden Kreis
umschreibender_kreis(kreis(P1,R1),kreis(P2,R2),kreis(P3,R3),KU) :-

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
	, RU is 1 / ( InvSum - 2 * InvRoot )

	% Konvertierung
	, baryzentrisch2kartesisch(BaryA/BaryB/BaryC, dreieck(P1,P2,P3), PU)

	, KU = kreis(PU, RU)
.

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


% Vektor zwischen zwei Punkten
abstand_vektor(X1/Y1,X2/Y2,X/Y) :-
  X is X1 - X2
  , Y is Y1 - Y2
.

% bewege Punkt X,Y mit dem Gradienten die Distanz D und bekomme Rx,Ry
move(D,gerade(X/Y,G),Rx/Ry) :-
  Tx is sqrt((D * D) / (1 + G * G))
  , Ty is G * Tx
  , (D < 0 ->
      Rx is X - Tx
      , Ry is Y - Ty
  ;
    Rx is X + Tx
    , Ry is Y + Ty)
.

% Berechnet zu zwei tangierenden Kreisen zwei die beiden tangierende Kreise mit Radius R3
tangierender_kreis_mit_radius(kreis(P1,R1),kreis(P2,R2),R3,kreis(P3A,R3),kreis(P3B,R3)) :-
  A is R2+R3
  , B is R3+R1
  , C is R1+R2
  , WBAC is acos((A*A - B*B - C*C) / (-2 * B * C))
  , abstand_vektor(P1,P2,X/Y)

  % Punkt P3A
  , Sin1 is sin(WBAC)
  , Cos1 is cos(WBAC)
  , X13A is X * Cos1 - Y * Sin1
  , Y13A is X * Sin1 + Y * Cos1
  , GA is Y13A / X13A
  , move(B,gerade(P1,GA),P3A)

  % Punkt P3B
  , Sin2 is sin(pi-WBAC)
  , Cos2 is cos(pi-WBAC)
  , X13B is X * Cos2 - Y * Sin2
  , Y13B is X * Sin2 + Y * Cos2
  , GB is Y13B / X13B
  , move(B,gerade(P1,GB),P3B)
.

% berechnet zweiten tangierenden Kreis zu drei tangierenden Kreisen
% falls man den ersten bereits berechnet hat
% benutzt Vietas Theorem welches bei einer Gleichung mit zwei Lösungen die
% zweite Lösung findet wenn man die Erste bereits hat
naechste_loesung(Kreis1,Kreis2,Kreis3,Vorherige,Naechste) :-

  % Radius berechnen
  maplist(radius,[Vorherige,Kreis1,Kreis2,Kreis3],Radien)
  , maplist(kehr,Radien,[LE,L1,L2,L3])
  , LZ is 2 * (L1 + L2 + L3) - LE
  , RZ is 1/LZ

  % Postiion berechnen
  , maplist(position,[Vorherige,Kreis1,Kreis2,Kreis3],[Xe/Ye,X1/Y1,X2/Y2,X3/Y3])
  , XZ is (2 * (L1*X1 + L2*X2 + L3*X3) - LE*Xe) / LZ
  , YZ is (2 * (L1*Y1 + L2*Y2 + L3*Y3) - LE*Ye) / LZ
  , Naechste = kreis(XZ/YZ,RZ)
.

mu(0.000001).

validate(kreis(P1,R1),kreis(P2,R2),kreis(P3,R3)) :-
  mu(Mu)

  , abstand(P1,P2,D12)
  , abstand(P2,P3,D23)
  , abstand(P1,P3,D13)

  , Mu > abs(D12-R1-R2)
  , Mu > abs(D23-R2-R3)
  , Mu > abs(D13-R1-R3)
.

/*
Unsere vorige Implementation hatte ein paar Fehler, also habe ich das Ganze
nochmal implementiert mit baue_kreise/7,schleife/7,durchlauf/6.
Die vorige Implementation findest du ganz unten.
*/

% berechnet Kreise
baue_kreise(
	Radius1
	,Radius2
	,Radius3
	,AnzahlGenerationen
	,MinimalerRadius
	,MaximaleKreisAnzahl
	,Kreise) :-

		Kreis1 = kreis(-Radius1/0,Radius1)
		, Kreis2 = kreis(Radius2/0,Radius2)
		, tangierender_kreis_mit_radius(Kreis1,Kreis2,Radius3,Kreis3,_)
		, umschreibender_kreis(Kreis1,Kreis2,Kreis3,KreisUmschreibend)
		, initiale_queue(Kreis1,Kreis2,Kreis3,KreisUmschreibend,InitialeQueue)
		, schleife(
				InitialeQueue
				, [KreisUmschreibend-0,Kreis1-0,Kreis2-0,Kreis3-0 | KreisEnde]-KreisEnde
				, 4
				, AnzahlGenerationen
				, MinimalerRadius
				, MaximaleKreisAnzahl
				, Kreise
				)
.

% Generiert die initiale Queue als Difference List
initiale_queue(
	Kreis1
	, Kreis2
	, Kreis3
	, KreisUmschreibend
	, [
		(Kreis1/Kreis2/Kreis3/KreisUmschreibend)-1 % Ergibt inbeschriebenen Kreis
		, (Kreis1/Kreis2/KreisUmschreibend/Kreis3)-1
		, (Kreis1/Kreis3/KreisUmschreibend/Kreis2)-1
		, (Kreis2/Kreis3/KreisUmschreibend/Kreis1)-1
	|QueueEnde]-QueueEnde
).

% leere Diff-List
empty(S-S) :- var(S).

% loop
schleife(
	QueueBisher
	, KreiseBisher
	, KreisAnzahl
	, AnzahlGenerationen
	, MinimalerRadius
	, MaximaleKreisAnzahl
	, KreiseResultat
) :-
	\+ empty(QueueBisher)
	, durchlauf(QueueBisher, KreiseBisher, QueueNeu, KreiseNeu, AnzahlGenerationen, MinimalerRadius)
	, (MaximaleKreisAnzahl < 0,!; KreisAnzahl =< MaximaleKreisAnzahl)
	, KreisAnzahlNeu is KreisAnzahl + 1
	, schleife(QueueNeu, KreiseNeu, KreisAnzahlNeu, AnzahlGenerationen, MinimalerRadius, MaximaleKreisAnzahl,KreiseResultat)
.
schleife(_,KreiseDiff-_,_,_,_,_,Kreise) :- zu_liste(KreiseDiff,Kreise).

% diff-list zu liste
zu_liste([H|T],[H|R]) :-
	ground(H)
	, zu_liste(T,R)
.
zu_liste(_,[]).

% eine iteration
durchlauf(
	QueueBisher-QueueBisherEnde
	, KreiseBisher-KreiseBisherEnde
	, QueueNeu-QueueNeuEnde
	, KreiseBisher-KreiseNeuEnde
	, AnzahlGenerationen
	, MinimalerRadius
) :-
	QueueBisher = [ (Kreis1/Kreis2/Kreis3/KreisLoesung)-Generation | QueueNeu ]
	, naechste_loesung(Kreis1,Kreis2,Kreis3,KreisLoesung,KreisNeu)
	, (AnzahlGenerationen < 0,!; Generation =< AnzahlGenerationen) % ist ja geordnet also können wir hier gleich aufhören
	,
	(
		(
		radius(KreisNeu,RadiusNeu)
		, (MinimalerRadius < 0,!; RadiusNeu >= MinimalerRadius) % radius muss nicht geordnet sein
		, KindGeneration is Generation + 1
		, QueueBisherEnde = [
				(KreisNeu/Kreis1/Kreis3/Kreis2)-KindGeneration
				,(KreisNeu/Kreis2/Kreis3/Kreis1)-KindGeneration
				,(KreisNeu/Kreis1/Kreis2/Kreis3)-KindGeneration
				| QueueNeuEnde
			]
		, KreiseBisherEnde = [ KreisNeu-Generation | KreiseNeuEnde ]
		);
		(
			KreiseNeuEnde = KreiseBisherEnde
			, QueueNeuEnde = QueueBisherEnde
		)
	)
.

% kreiert farbe:
% Key gibt den Modus an nach dem die Farbe kreiert werden soll
% Streckung gibt Wert an durch welchen P potenziert werden soll um die Verteilung mehr anzugleichen
%farbe_zuweisen(MaxRadius,Radius,MaxGeneration,Generation,Streckung,Key       ,InterpolationsFarben,Farbe)
farbe_zuweisen(_,_,_,_,_,_,[A],A).
farbe_zuweisen( _        ,_     ,_            ,_         ,_        ,random    ,InterpolationsFarben,Farbe) :-
	random(Wert)
	, interpolieren(InterpolationsFarben,Wert,Farbe)
.
farbe_zuweisen( MaxRadius,Radius,_            ,_         ,Streckung,radius    ,InterpolationsFarben,Farbe) :-
	P is (abs(Radius)/MaxRadius)^Streckung
	, interpolieren(InterpolationsFarben,max(0,min(1,P)),Farbe)
.
farbe_zuweisen( _        ,_     ,MaxGeneration,Generation,Streckung,generation,InterpolationsFarben,Farbe) :-
	P is (Generation/MaxGeneration)^Streckung
	, interpolieren(InterpolationsFarben,max(0,min(1,P)),Farbe)
.

interpolieren(F,T,P,R) :- R is round(F * (1 - P) + T * P).
interpolieren(Fr/Fg/Fb,Tr/Tg/Tb,P,Rr/Rg/Rb) :- interpolieren(Fr,Tr,P,Rr), interpolieren(Fg,Tg,P,Rg),interpolieren(Fb,Tb,P,Rb).
interpolieren(Vs,P,R) :-
	length(Vs,S)
	, Links is floor(P * (S-1))
	, Rechts is ceil(P * (S-1))
	, nth0(Links, Vs, WertLinks)
	, nth0(Rechts, Vs, WertRechts)
	, Prel is P - Links / (S - 1)
	, interpolieren(WertLinks,WertRechts,Prel,R)
.

% kreiert svg script für einen Kreis
kreis_zu_svg(MaxGeneration,MaxRadius,Key,Farbpalette,kreis(X/Y,Radius)-Generation,Str) :-
	farbe_zuweisen(MaxRadius,Radius,MaxGeneration,Generation,1/4,Key,Farbpalette,Cr/Cg/Cb)
	, RadiusBetrag is abs(Radius)
	, format(
		atom(Str)
		,'<circle cx="~5f" cy="~5f" r="~5f" stroke="none" stroke_width="1" fill="#~|~`0t~16r~2+~|~`0t~16r~2+~|~`0t~16r~2+" />\n', [X,Y,RadiusBetrag,Cr,Cg,Cb]
	)
.

% kreiert svg datei mit Kreisen
baue_svg(Kreise,Name,FarbenKey,MaxGeneration,MaxRadius,Farbpalette) :-
	open(Name, write, Out)
	, maplist(box, Kreise, Boxen)
	, foldl(max_fn, Boxen, [100000, -100000, 100000, -100000], [Links, Rechts, Oben, Unten])
	, LinksRounded = floor(Links)
	, ObenRounded = ceil(Oben)
	, Width is ceil(Rechts - Links)
	, Height is ceil(Unten - Oben)
	, maplist(kreis_zu_svg(MaxGeneration,MaxRadius,FarbenKey,Farbpalette), Kreise, Strings)
	, write(Out,'<?xml version="1.0" encoding="UTF-8"?><svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" baseProfile="full" width="20cm" height="20cm" ')
	, format(Out, 'viewBox="~d ~d ~d ~d">', [LinksRounded, ObenRounded, Width, Height])
	, maplist(write(Out), Strings)
	, write(Out, '</svg>')
	, close(Out)
	, !
.

max_fn([Links1, Rechts1, Oben1, Unten1], [Links2, Rechts2, Oben2, Unten2], [LinksR, RechtsR, ObenR, UntenR]) :-
	LinksR is min(Links1, Links2)
	, RechtsR is max(Rechts1, Rechts2)
	, ObenR is min(Oben1, Oben2)
	, UntenR is max(Unten1, Unten2)
.
box(kreis(X/Y,R)-_,[X-R,X+R,Y-R,Y+R]).

test :-
	% Wir bekommen als Argumente
	% Radius1, Radius2, Radius3, MaxGenerationen, MinimalerRadius, MaxKreisanzahl, Farbmodus (radius,generation oder random), name svg-datei
	baue_kreise(100,200,100,10,-1,-1,[Y-T|X])
	% hier Generationen herausfiltern
	% filter_generation([Y-T|X],2,5,[A-K|B])
	% radius(A,R)
	% baus_svg([A-K|B],'hans.svg',radius,4,abs(R))
	, radius(Y,R)
	, Farbpalette = [179/16/7, 20/75/224, 67/189/40]
	, baue_svg([Y-T|X],'hans.svg',random,4,abs(R),Farbpalette)
.
