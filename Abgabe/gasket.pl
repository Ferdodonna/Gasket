:- module(gasket, [gasket/14]).
:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(apply)).
:- use_module(gasket_math).
:- use_module(gasket_svg).

% Generiert die initiale Queue von Kreisen als Difference List
initiale_queue(
	Queue-QueueBisherEnde
	, Kreis1Original
	, Kreis2Original
	, Kreis3Original
	, KreisUmschreibend-Generation
	, RotationTerm
	, Queue-QueueNeuEnde
) :-
	Rotation is RotationTerm
	, rotiere_kreis(Kreis1Original, KreisUmschreibend, Rotation, Kreis1)
	, rotiere_kreis(Kreis2Original, KreisUmschreibend, Rotation, Kreis2)
	, rotiere_kreis(Kreis3Original, KreisUmschreibend, Rotation, Kreis3)
	, GenerationPlusEins is Generation + 1
	, QueueBisherEnde = [
		Kreis1-Generation % Ergibt Kreis 1
		, Kreis2-Generation % Ergibt Kreis 2
		, Kreis3-Generation % Ergibt Kreis 3
		, (Kreis1/Kreis2/Kreis3/KreisUmschreibend)-GenerationPlusEins % Ergibt inbeschriebenen Kreis
		, (Kreis1/Kreis2/KreisUmschreibend/Kreis3)-GenerationPlusEins % Ergibt Kreis 1 Lösung
		, (Kreis1/Kreis3/KreisUmschreibend/Kreis2)-GenerationPlusEins % Ergibt Kreis 2 Lösung
		, (Kreis2/Kreis3/KreisUmschreibend/Kreis1)-GenerationPlusEins % Ergibt Kreis 3 Lösung
		| QueueNeuEnde
	]
.


% Überprüft, ob Difference list leer ist
ist_leer(S-S) :- var(S).


% Führt eine Iteration durch:
% 1. Nimmt ein Kreis-Quartupel und berechnet daraus einen neuen Kreis
% 2. Packt generierten Kreis in die Liste von generierten Kreisen (ans Ende)
% 3. Legt 3 neue Kreis-Quartupel in die Queue
schleifen_iteration(
	QueueBisher-QueueBisherEnde
	, Kreise-KreiseBisherEnde
	, QueueNeu-QueueNeuEnde
	, Kreise-KreiseNeuEnde
	, AnzahlGenerationen
	, MinimalerRadius
	, NestedGasketFunktion
	, Radius1/Radius2/Radius3-Rotation
) :-
	% Nächstes Queue Element berechnen
	QueueBisher = [ QueueFront | QueueNeu ]

	% Kreis berechnen aus Quartupel
	, (
	QueueFront = (Kreis1/Kreis2/Kreis3/KreisLoesung)-Generation
	, naechste_loesung(Kreis1,Kreis2,Kreis3,KreisLoesung,KreisNeu)

	% Berechneter Radius noch zu groß, oder noch nicht genug Generationen generiert?
	, (

		(
			radius(KreisNeu, RadiusNeu)
			, (AnzahlGenerationen =< 0; Generation =< AnzahlGenerationen; MinimalerRadius > 0, MinimalerRadius =< RadiusNeu)
			, (MinimalerRadius =< 0; MinimalerRadius =< RadiusNeu; AnzahlGenerationen > 0, Generation =< AnzahlGenerationen)

			% Neue Queue Elemente hinzufügen
			, KindGeneration is Generation + 1
			, (
				% Rekursive Berechnung erwünscht, oder nicht?
				(
					QueueFront = _-_
					, QueueBisherEnde = [
						(KreisNeu/Kreis1/Kreis3/Kreis2)-KindGeneration
						, (KreisNeu/Kreis2/Kreis3/Kreis1)-KindGeneration
						, (KreisNeu/Kreis1/Kreis2/Kreis3)-KindGeneration
						| QueueTempEnde
					]
				)
				; (
					QueueFront = _+_
					, QueueBisherEnde = QueueTempEnde
				)
			)

			% Neuen Kreis in die Liste schreiben
			, KreiseBisherEnde = [ KreisNeu-Generation | KreiseNeuEnde ]

			% Nested Gasket aufrufen
			, call(NestedGasketFunktion, QueueNeu-QueueTempEnde, KreisNeu-KindGeneration, Radius1/Radius2/Radius3-Rotation, QueueNeu-QueueNeuEnde)
		)

		% Nein! => Queue-Quartupel überspringen
		; (
			KreiseNeuEnde = KreiseBisherEnde
			, QueueNeuEnde = QueueBisherEnde
		)
	)
	;
	(
	QueueFront = Kreis-Generation
	, radius(Kreis, Radius)
	, (AnzahlGenerationen =< 0; Generation =< AnzahlGenerationen; MinimalerRadius > 0, MinimalerRadius =< Radius)
	, (MinimalerRadius =< 0; MinimalerRadius =< Radius; AnzahlGenerationen > 0, Generation =< AnzahlGenerationen)
	, !
	, KreiseBisherEnde = [QueueFront | KreiseNeuEnde]
	, call(NestedGasketFunktion, QueueNeu-QueueBisherEnde, Kreis-Generation, Radius1/Radius2/Radius3-Rotation, QueueNeu-QueueNeuEnde)
	; (
		KreiseNeuEnde = KreiseBisherEnde
		, QueueNeuEnde = QueueBisherEnde
	)
	)
	)
.


% Macht eine Difference List zu einer normalen Liste (schneidet den potenziellen variablen Tail ab)
zu_liste([H|T],[H|R]) :- ground(H), zu_liste(T,R).
zu_liste(_,[]).


% Wiederholt iterationen solange, bis Queue leer ist
schleife(
	QueueBisher
	, KreiseBisher
	, KreisAnzahl
	, AnzahlGenerationen
	, MinimalerRadius
	, MaximaleKreisAnzahl
	, NestedGasketFunktion
	, Radius1/Radius2/Radius3-Rotation
	, KreiseResultat
) :-
	% Queue überprüfen, dass sie nicht leer ist und wir neuen Kreis generieren dürfen
	\+ ist_leer(QueueBisher)
	, (MaximaleKreisAnzahl =< 0, !; KreisAnzahl < MaximaleKreisAnzahl)
	, !
	, KreisAnzahlNeu is KreisAnzahl + 1

	% Iteration durchführen
	, schleifen_iteration(QueueBisher, KreiseBisher, QueueNeu, KreiseNeu, AnzahlGenerationen, MinimalerRadius, NestedGasketFunktion, Radius1/Radius2/Radius3-Rotation)

	% Nächste Iteration aufrufen
	, schleife(QueueNeu, KreiseNeu, KreisAnzahlNeu, AnzahlGenerationen, MinimalerRadius, MaximaleKreisAnzahl, NestedGasketFunktion, Radius1/Radius2/Radius3-Rotation, KreiseResultat)
.
schleife(_,KreiseDifferenceList-_,_,_,_,_,_,_,Kreise) :- zu_liste(KreiseDifferenceList, Kreise).

% Creates 3 circles in the supplied circle with equal relative radiuses as the surrounding gasket
nested_gasket_scaled(Queue-QueueEnde, kreis(_, KreisRadius)-_, _, Queue-QueueEnde) :- KreisRadius < 0, !.
nested_gasket_scaled(Queue-QueueBisherEnde, KreisNeu-GenerationNeu, Radius1/Radius2/Radius3-Rotation, Queue-QueueNeuEnde) :-
	KreisNeu = kreis(KreisNeuX/KreisNeuY, KreisNeuRadius)
	, initiale_kreise(Radius1, Radius2, Radius3, Kreis1, Kreis2, Kreis3)
	, umschreibender_kreis(Kreis1,Kreis2,Kreis3,kreis(KreisUmschreibendX/KreisUmschreibendY, KreisUmschreibendRadius))
	, Skalierung is -KreisNeuRadius/KreisUmschreibendRadius % -1, weil Umschreibendender Kreis negativen radius besitzt
	, skaliere_kreis(Kreis1, KreisUmschreibendX, KreisUmschreibendY, Skalierung, KreisNeuX, KreisNeuY, Kreis1Skaliert)
	, skaliere_kreis(Kreis2, KreisUmschreibendX, KreisUmschreibendY, Skalierung, KreisNeuX, KreisNeuY, Kreis2Skaliert)
	, skaliere_kreis(Kreis3, KreisUmschreibendX, KreisUmschreibendY, Skalierung, KreisNeuX, KreisNeuY, Kreis3Skaliert)
	, initiale_queue(Queue-QueueBisherEnde,Kreis1Skaliert,Kreis2Skaliert,Kreis3Skaliert,kreis(KreisNeuX/KreisNeuY, -KreisNeuRadius)-GenerationNeu,Rotation,Queue-QueueNeuEnde)
.

% Creates random 3 circles in the supplied circle
nested_gasket_random(Queue-QueueBisherEnde, KreisNeu-GenerationNeu, _, Queue-QueueNeuEnde) :-
	A is random_float^(1/6)
	, B is 1/A
	% Formula for boundaries of Radius3 found using "solve 1/a + 1/b + 1/x < 2 * sqrt(1/ab+1/ax+1/bx) for x" in wolfram alpha
	, AB_cube is A*A*A*B*B*B
	, A_minus_B_sqared is (A-B)*(A-B)
	, RandomWidth is 2 * sqrt(AB_cube/A_minus_B_sqared/A_minus_B_sqared)
	, RandomOffset is (A*A*B + A*B*B) / (A-B) / (A-B)
	, Minimum is RandomOffset - RandomWidth
	, Maximum is min(max(A, Minimum), RandomOffset + RandomWidth)
	, X is Maximum - random_float^6 * (Maximum - Minimum)
	, random_permutation([A,B,X], [Radius1, Radius2, Radius3])
	, Rotation is random_float * pi * 2
	, nested_gasket_scaled(Queue-QueueBisherEnde, KreisNeu-GenerationNeu, Radius1/Radius2/Radius3-Rotation, Queue-QueueNeuEnde)
.

% Turns nesting off
nested_gasket_off(Queue-QueueEnde, _, _, Queue-QueueEnde).


% Berechnet alle Kreise innerhalb der übergebenen Bedingungen
generiere_gasket(
	Radius1
	, Radius2
	, Radius3
	, AnzahlGenerationen
	, MinimalerRadius
	, MaximaleKreisAnzahl
	, NestedGasketFunktion
	, Radius1/Radius2/Radius3-Rotation
	, Kreise
) :-
	% Berechne Initiale Kreise
	initiale_kreise(Radius1, Radius2, Radius3, Kreis1, Kreis2, Kreis3)

	% Berechne den Umschreibenden Kreis
	, umschreibender_kreis(Kreis1,Kreis2,Kreis3,KreisUmschreibend)

	% Erzeuge die initiale Queue von Kreisen
	, initiale_queue(Queue-Queue,Kreis1,Kreis2,Kreis3,KreisUmschreibend-0,Rotation,QueueInitial)
	% Rufe Berechnungsschleife auf
	, schleife(
		QueueInitial
		, [KreisUmschreibend-1 | KreisEnde]-KreisEnde
		, 1
		, AnzahlGenerationen
		, MinimalerRadius
		, MaximaleKreisAnzahl
		, NestedGasketFunktion
		, Radius1/Radius2/Radius3-Rotation
		, Kreise
	)
.


% Filtert die übergebene liste von Kreisen danach, ob ihre jeweilige Generation generiert werden soll
filter_kreise_praedikat(GenerationenFilter, _-Generation) :- !, Generation in GenerationenFilter.
filter_kreise(Kreise, GenerationenFilter, KreiseGefiltert) :- include(filter_kreise_praedikat(GenerationenFilter), Kreise, KreiseGefiltert).

% Maximum als funktion
maximum(A, B, R) :- R is max(A, B).


% Gibt Generation von kreis zurück
generation(_-Generation,Generation).

% Generiert Gasket und schreibt es in eine .svg Datei
gasket(
	Radius1
	, Radius2
	, Radius3
	, Rotation
	, Ausgabepfad
	, Generationen
	, KreisAnzahl
	, MinimalerKreisRadius
	, Nesting
	, GenerationenFilter
	, Gasketfarbe
	, HintergrundFarbe
	, Farbmodus
	, Farbpalette
) :-
	% Generiere alle Kreise
	generiere_gasket(Radius1, Radius2, Radius3, Generationen, MinimalerKreisRadius, KreisAnzahl, Nesting, Radius1/Radius2/Radius3-Rotation, AlleKreise)
	, AlleKreise = [Kreis|Kreise]

	% Filter die generierten Kreise ggf.
	, filter_kreise(Kreise, GenerationenFilter, KreiseGefiltert)

	% Finde den größten Radius heraus
	, maplist(absoluter_radius, Kreise, KreisRadii)
	, foldl(maximum, KreisRadii, 0, MaximalerRadius)

	% Finde die größte Generation heraus
	, maplist(generation, Kreise, KreisGenerationen)
	, foldl(maximum, KreisGenerationen, 0, MaximaleGenerationen)

	% Schreibe SVG Datei
	, !
	, schreibe_svg([Kreis|KreiseGefiltert], Ausgabepfad, MaximaleGenerationen, MaximalerRadius, GenerationenFilter, Gasketfarbe, HintergrundFarbe, Farbmodus, Farbpalette)
.
