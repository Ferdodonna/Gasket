
/*
TODO:
- convert to cplfd (clpfd with floats?)
- complete
- validate

*/

:- use_module(library(clpfd)).

:- dynamic circle/2.
% circle(radius,coordinates)
:- dynamic line/2.

% line(gradient,coordiantes)



% checks if that each of the 3 circles touches each of the other 2 in one exactly one point
valid(circle(R1,C1),circle(R2,C2),circle(R3,C3)) :-
  dist(C1,C2,D12), % need to round D12, D13, D23
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

% finds the middle of 3 circles, meaning the point that has the same distance to each of the circle fringes
middle(circle(R1,C1),circle(R2,C2),circle(R3,C3),M) :-
  touchpoint(circle(R1,C1),circle(R2,C2),TP12),
  touchpoint(circle(R2,C2),circle(R3,C3),TP23),
  createLine(C1,TP23,L23),
  createLine(C3,TP12,L12),
  crossing(L12,L23,M).

% finds the touching point of two circles, provided they touch (no intersection)
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
move(D,line(Grad,X-Y),RX-RY) :-
  D >= 0,
  XD is sqrt((D * D) / (Grad + 1)),
  YD is sqrt((D * D) - (XD * XD)),
  RX is X + XD,
  RY is Y + YD.



























%
