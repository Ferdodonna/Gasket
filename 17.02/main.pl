:- use_module(library(optparse)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(gasket)).

% Zeigt Hilfe an
hilfe_anzeigen(KommandoZeile, OptionenSpezifikation) :-
	write('Usage: ') , nl
	, KommandoZeile = [Programmname|_]
	, write('  ') , write(Programmname) , write(' [flags..] <Radius-1> <Radius-2> <Radius-3> [Farbpalette im Format R/G/B...]') , nl , nl
	, write('Flags:') , nl
	, opt_help(OptionenSpezifikation, HelpText)
	, write(HelpText)
.

% Generiert Gasket und schreibt es in eine .svg Datei
generate(
	Radius1
	, Radius2
	, Radius3
	, Ausgabepfad
	, Generationen
	, KreisAnzahl
	, MinimalerKreisRadius
	, GenerationenFilter
	, HintergrundFarbe
	, GasketFarbe
	, FarbenAlgorithmus % farbe_radius, farbe_generation, farbe_wahllos, farbe_wahllos_diskret
	, Farbpalette
) :-
	write(Ausgabepfad)
	, write(', ')
	, write(Generationen)
	, write(', ')
	, write(KreisAnzahl)
	, write(', ')
	, write(MinimalerKreisRadius)
	, write(', ')
	, write(GenerationenFilter)
	, write(', ')
	, write(HintergrundFarbe)
	, write(', ')
	, write(GasketFarbe)
	, write(', ')
	, write(FarbenAlgorithmus)
	, write(', ')
	, write(Farbpalette)
.

% Programmstart
main(KommandoZeile) :-
	!
	% Mögliche Optionen Spezifizieren
	, OptionenSpezifikation = [
		[opt(ausgabepfad), shortflags([o]), longflags([output]), default('gasket.svg'), help('Name der zu erzeugenden .svg Datei.')]
		, [opt(hilfe), shortflags([h]), longflags([help]), type(boolean), default('false'), help('Zeigt diese Hilfe an.')]
		, [opt(generationen), shortflags([g]), longflags([generations]), type(integer), default(0), help('Anzahl der Generationen von Kreisen.')]
		, [opt(kreis_anzahl), shortflags([n]), longflags([total]), type(integer), default(0), help('Gesamtzahl von zu erzegenden Kreisen.')]
		, [opt(minimaler_kreis_radius), shortflags([r]), longflags([radius]), type(integer), default(0), help('Radius-Limit, bis zu dem Kreise generiert werden sollen.')]
		, [opt(generationen_filter), shortflags([f]), longflags([filter]), default(inf..sup), help('Beschreibung von Kreis-Generationen, die in .svg geschrieben werden sollen. Beispiel: ''2..sup''.')]
		, [opt(hintergrund_farbe), longflags([background]), default('255/255/255'), help('Hintergrund Farbe der .svg Datei (Format: ''R/G/B'' jeweils im Bereich 0..255).')]
		, [opt(gasket_farbe), longflags([gasket]), default('0/0/0'), help('Hintergrund Farbe der .svg Datei (Format: ''R/G/B'' jeweils im Bereich 0..255).')]
		, [opt(farbe_radius), longflags(['radius-coloring']), type(boolean), default('false'), help('Stellt den modus es Einfärbens auf ''radius-basierend''.')]
		, [opt(farbe_generation), longflags(['generation-coloring']), type(boolean), default('false'), help('Stellt den modus es Einfärbens auf ''generation-basierend''.')]
		, [opt(farbe_wahllos), longflags(['random-coloring']), type(boolean), default('false'), help('Stellt den Modus es Einfärbens auf zufällig aus der Palette, jedoch werden Farben interpoliert.')]
		, [opt(farbe_wahllos_diskret), longflags(['random-coloring-discrete']), type(boolean), default('false'), help('Stellt den Modus es Einfärbens auf zufällig aus der Palette.')]
	]
	
	% Kommandozeile parsen
	, opt_parse(OptionenSpezifikation, KommandoZeile, Optionen, PositionaleArgumente)
	
	% Entweder Hilfe anzeigen oder Programmlogik aufrufen
	, (
		( % Hilfe angefordert bzw. falscher Aufruf
			member(hilfe(true), Optionen)
			; \+ PositionaleArgumente = [_, _, _, _|_]
		)
			, hilfe_anzeigen(KommandoZeile, OptionenSpezifikation)
		; (
			% Optionen auslesen
			member(ausgabepfad(Ausgabepfad), Optionen)
			, member(generationen(Generationen), Optionen)
			, member(kreis_anzahl(KreisAnzahl), Optionen)
			, member(minimaler_kreis_radius(MinimalerKreisRadius), Optionen)
			, member(generationen_filter(GenerationenFilter), Optionen)
			, member(hintergrund_farbe(HintergrundFarbe), Optionen)
			, member(gasket_farbe(GasketFarbe), Optionen)
			
			% Färbealgorithmus bestimmen
			, (member(farbe_radius(true), Optionen), FarbenAlgorithmus = farbe_radius; true)
			, (member(farbe_generation(true), Optionen), FarbenAlgorithmus = farbe_generation; true)
			, (member(farbe_wahllos(true), Optionen), FarbenAlgorithmus = farbe_wahllos; true)
			, (member(farbe_wahllos_diskret(true), Optionen), FarbenAlgorithmus = farbe_wahllos_diskret; true)
			, (FarbenAlgorithmus = farbe_wahllos; true)
			
			% Farbpalette bestimmen
			, PositionaleArgumente = [_, Radius1, Radius2, Radius3|Farbpalette]
			, (Farbpalette = [], FarbpaletteNormalisiert = [255/255/255]; FarbpaletteNormalisiert = Farbpalette)
			
			% Generierung aufrufen
			, generate(
				Radius1
				, Radius2
				, Radius3
				, Ausgabepfad
				, Generationen
				, KreisAnzahl
				, MinimalerKreisRadius
				, GenerationenFilter
				, HintergrundFarbe
				, GasketFarbe
				, FarbenAlgorithmus
				, FarbpaletteNormalisiert
			)
		)
	)
.
