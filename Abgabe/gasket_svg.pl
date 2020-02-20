:- module(gasket_svg, [schreibe_svg/9]).

:- use_module(library(clpfd)).

% Interpoliert zwei oder eine Liste von Farben
farbe_interpolieren(VonRot/VonGruen/VonBlau,NachRot/NachGruen/NachBlau,Prozentsatz,ErgebnisRot/ErgebnisGruen/ErgebnisBlau) :-
	!
	, farbe_interpolieren(VonRot,NachRot,Prozentsatz,ErgebnisRot)
	, farbe_interpolieren(VonGruen,NachGruen,Prozentsatz,ErgebnisGruen)
	, farbe_interpolieren(VonBlau,NachBlau,Prozentsatz,ErgebnisBlau)
.
farbe_interpolieren(Von,Nach,Prozentsatz,Ergebnis) :- Ergebnis is round(Von * (1 - Prozentsatz) + Nach * Prozentsatz).
farbe_interpolieren(Farben,Prozentsatz,Farbe) :-
	length(Farben, AnzahlFarben)
	, IndexLinks is floor(Prozentsatz * (AnzahlFarben - 1))
	, IndexRechts is ceil(Prozentsatz * (AnzahlFarben - 1))
	, nth0(IndexLinks, Farben, FarbeLinks)
	, nth0(IndexRechts, Farben, FarbeRechts)
	, ProzentsatzRelativ is ( Prozentsatz - IndexLinks / (AnzahlFarben - 1) ) * (AnzahlFarben - 1)
	, farbe_interpolieren(FarbeLinks,FarbeRechts,ProzentsatzRelativ,Farbe)
.

% Berechnet zu einem gegebenen Kreis das Rechteck, das ihn umfasst
umfassendes_rechteck(kreis(X/Y,R)-_,[X-abs(R),X+abs(R),Y-abs(R),Y+abs(R)]).

% Berechnet von zwei Rechtecken das Rechteck, das beide umfasst
umfassendes_rechteck([Links1, Rechts1, Oben1, Unten1], [Links2, Rechts2, Oben2, Unten2], [LinksR, RechtsR, ObenR, UntenR]) :-
	LinksR is min(Links1, Links2)
	, RechtsR is max(Rechts1, Rechts2)
	, ObenR is min(Oben1, Oben2)
	, UntenR is max(Unten1, Unten2)
.


% Weist den übergebenen Kreisinformationen je nach Einfärbungsmodus eine Farbe zu
berechne_farbe(_				,_		,_					,_			,zufall							,_			,R/G/B) :- !, random(0,255,R), random(0,255,G), random(0,255,B).
berechne_farbe(_				,_		,_					,_			,_								,[A]		,A) :- !.
berechne_farbe(_				,_		,_			  		,_		 	,zufall_palette					,Farbpalette,Farbe)
	:- !, length(Farbpalette,Laenge), random(0,Laenge,Index), nth0(Index,Farbpalette,Farbe).
berechne_farbe(_				,_		,_			 		,_			,zufall_palette_interpolierend	,Farbpalette,Farbe) :- !, Wert is random_float, farbe_interpolieren(Farbpalette,Wert,Farbe).
berechne_farbe(_				,_		,_			 		,Generation	,generation						,Farbpalette,Farbe)
	:- !, length(Farbpalette,Laenge), Index is mod(Generation, Laenge), nth0(Index, Farbpalette, Farbe).
berechne_farbe(_				,_		,MaximaleGeneration	,Generation	,generation_interpolierend		,Farbpalette,Farbe)
	:- Prozentsatz is Generation/MaximaleGeneration, farbe_interpolieren(Farbpalette,max(0,min(1,Prozentsatz)),Farbe).
berechne_farbe(MaximalerRadius	,Radius	,_					,_			,radius_interpolierend			,Farbpalette,Farbe)
	:- !, Prozentsatz is (abs(Radius)/MaximalerRadius)^(1/4), farbe_interpolieren(Farbpalette,max(0,min(1,1-Prozentsatz)),Farbe).


% Konvertiert den übergebenen Kreis in SVG Code
kreis_zu_svg(MaximaleGeneration,MaximalerRadius,Farbmodus,Farbpalette,kreis(X/Y,Radius)-Generation,Svg) :-
	!
	, berechne_farbe(MaximalerRadius,Radius,MaximaleGeneration,Generation,Farbmodus,Farbpalette,Cr/Cg/Cb)
	, RadiusBetrag is abs(Radius)
	, format(atom(Svg),'<circle cx="~5f" cy="~5f" r="~5f" stroke="none" stroke_width="1" fill="#~|~`0t~16r~2+~|~`0t~16r~2+~|~`0t~16r~2+" />\n', [X,Y,RadiusBetrag,Cr,Cg,Cb])
.

negativer_radius(kreis(_,Radius)-_) :- Radius < 0.

% Konveriert die übergebene Liste von Kreisen in SVG Code und schreibt diesen an den angegebenen Dateipfad
schreibe_svg(
	AlleKreise
	, Name
	, MaximaleGeneration
	, MaximalerRadius
	, GenerationenFilter
	, Gasketfarbe
	, Hintergrundfarbe
	, Farbmodus
	, Farbpalette
) :-
	!
	
	% Bounding Boxen von allen Kreisen erzeugen
	, maplist(umfassendes_rechteck, AlleKreise, Boxen)
	
	% Bounding Box um den kompletten Gasket ermitteln
	, foldl(umfassendes_rechteck, Boxen, [100000, -100000, 100000, -100000], [Links, Rechts, Oben, Unten])
	
	% Viewpoint der SVG Datei berechnen
	, Width is Rechts - Links
	, Height is Unten - Oben
	
	% Kreise in SVG Code konvertieren
	, AlleKreise = [Kreis|Kreise]
	, include(negativer_radius, Kreise, KreiseNegativerRadius)
	, exclude(negativer_radius, Kreise, KreisePositiverRadius)
	, kreis_zu_svg(_,_,zufall_palette,[Gasketfarbe], Kreis, KreisSVGCode)
	, maplist(kreis_zu_svg(MaximaleGeneration,MaximalerRadius,Farbmodus,Farbpalette), KreiseNegativerRadius, KreiseNegativSVGCode)
	, maplist(kreis_zu_svg(MaximaleGeneration,MaximalerRadius,Farbmodus,Farbpalette), KreisePositiverRadius, KreisePositivSVGCode)
	
	% SVG Datei schreiben
	, open(Name, write, Out)
	, write(Out,'<?xml version="1.0" encoding="UTF-8"?><svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" baseProfile="full" width="20cm" height="20cm" ')
	, ( Hintergrundfarbe \= 255/255/255, Hintergrundfarbe = R/G/B, format(Out, 'style="background-color:#~|~`0t~16r~2+~|~`0t~16r~2+~|~`0t~16r~2+" ', [R,G,B]); true)
	, format(Out, 'viewBox="~f ~f ~f ~f">\n', [Links, Oben, Width, Height])
	
	% Kreise schreiben
	, maplist(write(Out), KreiseNegativSVGCode)
	, (Kreis = kreis(_,_)-KreisGeneration, KreisGeneration in GenerationenFilter, write(Out, KreisSVGCode), !; true) % Gasket Kreis nur generieren, wenn die entsprechende Generation erlaubt ist
	, maplist(write(Out), KreisePositivSVGCode)
	, write(Out, '</svg>')
	, close(Out)
.
