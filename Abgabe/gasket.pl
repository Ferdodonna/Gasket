:- module(gasket, [gasket/12]).
:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(apply)).
:- use_module(gasket_math).
:- use_module(gasket_svg).

% Generiert die initiale Queue von Kreisen als Difference List
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


% Überprüft, ob Difference list leer ist
ist_leer(S-S) :- var(S).


% Führt eine Iteration durch:
% 1. Nimmt ein Kreis-Quartupel und berechnet daraus einen neuen Kreis
% 2. Packt generierten Kreis in die Liste von generierten Kreisen (ans Ende)
% 3. Legt 3 neue Kreis-Quartupel in die Queue
schleifen_iteration(
	QueueBisher-QueueBisherEnde
	, KreiseBisher-KreiseBisherEnde
	, QueueNeu-QueueNeuEnde
	, KreiseBisher-KreiseNeuEnde
	, AnzahlGenerationen
	, MinimalerRadius
) :-
	% Nächstes Queue Element berechnen
	QueueBisher = [ (Kreis1/Kreis2/Kreis3/KreisLoesung)-Generation | QueueNeu ]
	
	% Kreis berechnen aus Quartupel
	, naechste_loesung(Kreis1,Kreis2,Kreis3,KreisLoesung,KreisNeu)
	
	% Berechneter Radius noch zu groß, oder noch nicht genug Generationen generiert?
	, (
		(
			radius(KreisNeu, RadiusNeu)
			, (AnzahlGenerationen =< 0; Generation =< AnzahlGenerationen; MinimalerRadius > 0, MinimalerRadius =< RadiusNeu)
			, (MinimalerRadius =< 0; MinimalerRadius =< RadiusNeu; AnzahlGenerationen > 0, Generation =< AnzahlGenerationen)
			
			% Neue Queue Elemente hinzufügen
			, KindGeneration is Generation + 1
			, QueueBisherEnde = [
					(KreisNeu/Kreis1/Kreis3/Kreis2)-KindGeneration
					, (KreisNeu/Kreis2/Kreis3/Kreis1)-KindGeneration
					, (KreisNeu/Kreis1/Kreis2/Kreis3)-KindGeneration
					| QueueNeuEnde
				]
				
			% Neuen Kreis in die Liste schreiben
			, KreiseBisherEnde = [ KreisNeu-Generation | KreiseNeuEnde ]
		)
		
		% Nein! => Queue-Quartupel überspringen
		; (
			KreiseNeuEnde = KreiseBisherEnde
			, QueueNeuEnde = QueueBisherEnde
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
	, KreiseResultat
) :-
	% Queue überprüfen, dass sie nicht leer ist und wir neuen Kreis generieren dürfen
	\+ ist_leer(QueueBisher)
	, (MaximaleKreisAnzahl =< 0, !; KreisAnzahl =< MaximaleKreisAnzahl)
	, !
	, KreisAnzahlNeu is KreisAnzahl + 1
	
	% Iteration durchführen
	, schleifen_iteration(QueueBisher, KreiseBisher, QueueNeu, KreiseNeu, AnzahlGenerationen, MinimalerRadius)
	
	% Nächste Iteration aufrufen
	, schleife(QueueNeu, KreiseNeu, KreisAnzahlNeu, AnzahlGenerationen, MinimalerRadius, MaximaleKreisAnzahl, KreiseResultat)
.
schleife(_,KreiseDifferenceList-_,_,_,_,_,Kreise) :- zu_liste(KreiseDifferenceList, Kreise).


% Berechnet alle Kreise innerhalb der übergebenen Bedingungen
generiere_gasket(
	Radius1
	, Radius2
	, Radius3
	, AnzahlGenerationen
	, MinimalerRadius
	, MaximaleKreisAnzahl
	, Kreise
) :-
	% Berechne Initiale Kreise
	initiale_kreise(Radius1, Radius2, Radius3, Kreis1, Kreis2, Kreis3)
	
	% Berechne den Umschreibenden Kreis
	, umschreibender_kreis(Kreis1,Kreis2,Kreis3,KreisUmschreibend)
	
	% Erzeuge die initiale Queue von Kreisen
	, initiale_queue(Kreis1,Kreis2,Kreis3,KreisUmschreibend,InitialeQueue)
	
	% Rufe Berechnungsschleife auf
	, schleife(
		InitialeQueue
		, [KreisUmschreibend-1,Kreis1-0,Kreis2-0,Kreis3-0 | KreisEnde]-KreisEnde
		, 4
		, AnzahlGenerationen
		, MinimalerRadius
		, MaximaleKreisAnzahl
		, Kreise
	)
.


% Filtert die übergebene liste von Kreisen danach, ob ihre jeweilige Generation generiert werden soll
filter_kreise_praedikat(GenerationenFilter, _-Generation) :- !, Generation in GenerationenFilter.
filter_kreise(Kreise, GenerationenFilter, KreiseGefiltert) :- include(filter_kreise_praedikat(GenerationenFilter), Kreise, KreiseGefiltert).

% Gibt den absoluten Radius eines generierten Kreises zurück
absoluter_radius(kreis(_,Radius)-_, AbsoluterRadius) :- AbsoluterRadius is abs(Radius).


% Maximum als funktion
maximum(A, B, R) :- R is max(A, B).


% Gibt Generation von kreis zurück
generation(_-Generation,Generation).

% Generiert Gasket und schreibt es in eine .svg Datei
gasket(
	Radius1
	, Radius2
	, Radius3
	, Ausgabepfad
	, Generationen
	, KreisAnzahl
	, MinimalerKreisRadius
	, GenerationenFilter
	, Gasketfarbe
	, HintergrundFarbe
	, Farbmodus
	, Farbpalette
) :-
	% Generiere alle Kreise
	generiere_gasket(Radius1, Radius2, Radius3, Generationen, MinimalerKreisRadius, KreisAnzahl, AlleKreise)
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
