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
naechste_loesung(K1,K2,K3,Vorherige,Naechste) :-

  % Radius berechnen
  maplist(radius,[Vorherige,K1,K2,K3],Radien)
  , maplist(kehr,Radien,[LE,L1,L2,L3])
  , LZ is 2 * (L1 + L2 + L3) - LE
  , RZ is 1/LZ

  % Postiion berechnen
  , maplist(position,[Vorherige,K1,K2,K3],[Xe/Ye,X1/Y1,X2/Y2,X3/Y3])
  , XZ is (2 * (L1*X1 + L2*X2 + L3*X3) - LE*Xe) / LZ
  , YZ is (2 * (L1*Y1 + L2*Y2 + L3*Y3) - LE*Ye) / LZ
  , Naechste = kreis(XZ/YZ,RZ)
.

loopstart(K1,K2,K3,Depth) :-
  validate(K1,K2,K3)
  , findCenters(K1,K2,K3,Outer)
  , maplist(assert,[K1,K2,K3,Outer])
  , loop(K1,K2,K3,Outer,Depth)
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

% Does one iteration
iterate([(K1/K2/K3/Kg)|Qs],Ks,[Kn|Ks],QueueNeu) :-
	naechste_loesung(K1,K2,K3,Kg,Kn)
	, append(Qs,[(K1/K3/Kn/K2),(K2/K3/Kn/K1),(K1/K2/Kn/K3)],QueueNeu)
.

iterate_n(Queue,Kreise,Kreise,Queue,0).
iterate_n(Queue,Kreise,KreiseNeu,QueueNeu,N) :-
	iterate(Queue,Kreise,KreiseTemp,QueueTemp)
	, N1 is N-1
	, iterate_n(QueueTemp,KreiseTemp,KreiseNeu,QueueNeu,N1)
.

initiale_queue(K1,K2,K3,KU,[(K1/K2/K3/KU), (K1/K2/KU/K3), (K1/KU/K3/K2), (KU/K2/K3/K1)]).

generate(N,Kreise) :-
	K1 = kreis(400/400,100)
	, K2 = kreis(600/400,100)
	, tangierender_kreis_mit_radius(K1,K2,30,K3,_)
	%, K3 = kreis(500/600,123.61)
	, umschreibender_kreis(K1,K2,K3,KU)
	, initiale_queue(K1,K2,K3,KU,Queue)
	, iterate_n(Queue,[K1,K2,K3,KU],Kreise,_,N)
	, !
.

interpolate(F,T,P,R) :- R is round(F * (1 - P) + T * P).
interpolate(Fr/Fg/Fb,Tr/Tg/Tb,P,Rr/Rg/Rb) :- interpolate(Fr,Tr,P,Rr), interpolate(Fg,Tg,P,Rg),interpolate(Fb,Tb,P,Rb).
interpolate(Vs,P,R) :-
	length(Vs,S)
	, Links is floor(P * (S-1))
	, Rechts is ceil(P * (S-1))
	, nth0(Links, Vs, WertLinks)
	, nth0(Rechts, Vs, WertRechts)
	, Prel is P - Links / (S - 1)
	, interpolate(WertLinks,WertRechts,Prel,R)
.
	
color(P,C) :-
	Farben = [179/16/7, 20/75/224, 67/189/40]
	, interpolate(Farben, min(1,P), C)
.

kreis2svg(ColorFn,Rmax,kreis(X/Y,R),Str) :-
	R > 0
	, call(ColorFn, (R/Rmax)^(1/4), Cr/Cg/Cb)
	, format(atom(Str),'<circle cx="~5f" cy="~5f" r="~5f" stroke="none" stroke_width="1" fill="#~|~`0t~16r~2+~|~`0t~16r~2+~|~`0t~16r~2+" />\n', [X,Y,R,Cr,Cg,Cb])
.

kreis2svg(_,_,kreis(_,R),Str) :- R < 0, Str = ''.

max_fn([Links1, Rechts1, Oben1, Unten1], [Links2, Rechts2, Oben2, Unten2], [LinksR, RechtsR, ObenR, UntenR]) :-
	LinksR is min(Links1, Links2)
	, RechtsR is max(Rechts1, Rechts2)
	, ObenR is min(Oben1, Oben2)
	, UntenR is max(Unten1, Unten2)
.
box(kreis(X/Y,R),[X-R,X+R,Y-R,Y+R]).

generate_svg(N, Name) :-
	open(Name,write,Out)
	, generate(N,Kreise)
	, maplist(box, Kreise, Boxen)
	, foldl(max_fn, Boxen, [100000, -100000, 100000, -100000], [Links, Rechts, Oben, Unten])
	, write([Links, Rechts, Oben, Unten])
	, LinksRounded = floor(Links)
	, ObenRounded = floor(Oben)
	, Width is ceil(Rechts - Links)
	, Height is ceil(Unten - Oben)
	, maplist(kreis2svg(color,234), Kreise, Strings)
	, write(Out,'<?xml version="1.0" encoding="UTF-8"?><svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" baseProfile="full" width="20cm" height="20cm" ')
	, format(Out, 'viewBox="~d ~d ~d ~d">', [LinksRounded, ObenRounded, Width, Height])
	, maplist(write(Out), Strings)
	, write(Out, '</svg>')
	, close(Out)
	, !
.
