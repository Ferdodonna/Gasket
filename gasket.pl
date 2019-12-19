
:- use_module(library(clpfd)).

:- dynamic circle/2.
% circle(radius,coordinates)
:- dynamic line/2.
% line(gradient,coordiantes)



% checks if that each of the 3 circles touches each of the other 2 in one exactly one point
valid(circle(R1,C1),circle(R2,C2),circle(R3,C3)) :-
  dist(C1,C2,D12),
  dist(C1,C3,D13),
  dist(C2,C3,D23),
  R1+R2 #= D12,
  R1+R3 #= D13,
  R2+R3 #= D23.


% given two coordiantes calculates the positive distance between them
dist(X1-Y1,X2-Y2,Result) :-
  X is abs(X1-X2),
  Y is abs(Y1-Y2),
  Result is sqrt(X*X + Y*Y).

% given two lines, checks if they are orthogonal
orthogonal(line(Grad,C),line(O_Grad,C)) :-
  O_Grad is (-1)*(1/Grad).

% checks if a point is a point of intersection for two lines
crossing(line(Grad1,X1-Y1),line(Grad2,X2-Y2),X-Y) :-
  Grad1 \= Grad2,
  YZ1 is Y1 - X1 * Grad1,
  YZ2 is Y2 - X2 * Grad2,
  X is (YZ2 - YZ1) / (Grad1- Grad2),
  Y is X*Grad1 + YZ1.

% creates a line that goes through two given points
createLine(X1-Y1,X2-Y2,line(Gradient,X1-Y1)) :-
  Gradient is (Y1-Y2) / (X1-X2).

% finds the touching point of two circles, provided they touch
touchpoint(circle(R1,C1),circle(R2,C2),TP) :-
  C1 = X1-_,
  C2 = X2-_,
  (
  X1 > X2
  -> createLine(C2,C1,L12)
  ,  move(R2,L12,TP)
  ;  createLine(C1,C2,L12)
  ,  move(R1,L12,TP)
  ).


% move a point a certain distance given a gradient
% move(D,line(G,P),T) => T is the point P moved a distance of D along the gradient G
move(D,line(G,X-Y),Rx-Ry) :-
  Tx is sqrt((D * D) / (1 + G * G)),
  Ty is G * Tx,
  (D < 0 ->
    Rx is X - Tx,
    Ry is Y - Ty
  ;
    Rx is X + Tx,
    Ry is Y + Ty).

% Mittelpunkt zwischen zwei Punkten
midpoint(X1-Y1,X2-Y2,X-Y) :-
  X is (X1 + X2) / 2,
  Y is (Y1 + Y2) / 2.

% berechnet die Mitte nach deiner Vermutung über den Dreiecksschwerpunkt
middle(C1,C2,C3,S1,S2,S3) :-
  touchpoint(C1,C2,TP12),
  touchpoint(C2,C3,TP23),
  touchpoint(C3,C1,TP31),

  midpoint(TP12,TP31,M1),
  midpoint(TP12,TP23,M2),
  midpoint(TP23,TP31,M3),

  createLine(M1,TP23,L1),
  createLine(M2,TP31,L2),
  createLine(M3,TP12,L3),

  crossing(L1,L2,S1),
  crossing(L2,L3,S2),
  crossing(L3,L1,S3).

/*
Testlauf:

?- C1 = circle(2,2-0), C2 = circle(2,(-2)-0), dist(0-5,2-0,X), R is X - 2, C3 = circle(R,0-5), middle(C1,C2,C3,S1,S2,S3).
C1 = circle(2, 2-0),
C2 = circle(2, -2-0),
X = 5.385164807134504,
R = 3.3851648071345037,
C3 = circle(3.3851648071345037, 0-5),
S1 = -2.254972746618902e-16-1.2379689211803457,
S2 = -7.401486830834378e-17-1.237968921180346,
S3 = -7.401486830834377e-17-1.2379689211803457.

Also entweder ist mein code falsch oder das stimmt so nicht ganz, da ja der Schnittpunkt S1 anders ist als die anderen auch wenn nur minimal.

*/

%
