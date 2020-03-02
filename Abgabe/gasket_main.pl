:- use_module(library(optparse)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(gasket).

% Zeigt Hilfe an
hilfe_anzeigen(KommandoZeile, OptionenSpezifikation) :-
	write('Usage: ') , nl
	, KommandoZeile = [Programmname|_]
	, write('  ') , write(Programmname) , write(' [flags..] <Radius-1> <Radius-2> <Radius-3> [Farbpalette im Format R/G/B...]') , nl , nl
	, write('Flags:') , nl
	, opt_help(OptionenSpezifikation, HelpText)
	, write(HelpText)
.

% Programmstart
main(KommandoZeile) :-
	set_prolog_flag(stack_limit, 6_000_000_000)
	
	% Mögliche Optionen Spezifizieren
	, OptionenSpezifikation = [
		[opt(ausgabepfad), shortflags([o]), longflags([output]), default('gasket.svg'), help('Name der zu erzeugenden .svg Datei.')]
		, [opt(hilfe), shortflags([h]), longflags([help]), type(boolean), default('false'), help('Zeigt diese Hilfe an.')]
		, [opt(generationen), shortflags([g]), longflags([generations]), type(integer), default(5), help('Anzahl der Generationen von Kreisen.')]
		, [opt(kreis_anzahl), shortflags([n]), longflags([total]), type(integer), default(0), help('Gesamtzahl von zu erzegenden Kreisen.')]
		, [opt(minimaler_kreis_radius), shortflags([r]), longflags([radius]), type(term), default(0), help('Radius-Limit, bis zu dem Kreise generiert werden sollen.')]
		, [opt(generationen_filter), shortflags([f]), longflags([filter]), default(inf..sup), help('Beschreibung von Kreis-Generationen, die in .svg geschrieben werden sollen. Beispiel: ''2..sup''.')]
		, [opt(hintergrund_farbe), longflags([background]), default(255/255/255), help('Hintergrund Farbe der .svg Datei (Format: ''R/G/B'' jeweils im Bereich 0..255).')]
		, [opt(gasket_farbe), longflags([gasket]), default(0/0/0), help('Hintergrund Farbe der .svg Datei (Format: ''R/G/B'' jeweils im Bereich 0..255).')]
		, [opt(farbe_zufall), longflags(['random-coloring']), type(boolean), default('false'), help('Stellt den Modus es Einfärbens auf zufällig aus der Palette.')]
		, [opt(farbe_zufall_palette_interpolierend), longflags(['random-coloring-interpolated']), type(boolean), default('false'), help('Stellt den Modus es Einfärbens auf zufällig aus der Palette.')]
		, [opt(farbe_generation), longflags(['generation-coloring']), type(boolean), default('false'), help('Stellt den modus es Einfärbens auf ''generation-basierend''.')]
		, [opt(farbe_generation_interpolierend), longflags(['generation-coloring-interpolated']), type(boolean), default('false'), help('Stellt den modus es Einfärbens auf ''generation-basierend'', jedoch interpolierend.')]
		, [opt(farbe_radius_interpolierend), longflags(['radius-coloring']), type(boolean), default('false'), help('Stellt den modus es Einfärbens auf ''radius-basierend''.')]
		, [opt(nested_random), longflags(['random-nesting']), type(boolean), default('false'), help('Aktiviert zufallsbasiertes nesting von Gaskets.')]
		, [opt(nested_scaled), longflags(['scaled-nesting']), type(boolean), default('false'), help('Aktiviert nesting von Gaskets mithilfe von skalierung.')]
		, [opt(nested_relfected), longflags(['reflecting-nesting']), type(boolean), default('false'), help('Aktiviert nesting von Gaskets mithilfe von Reflektierung.')]
		, [opt(rotation), longflags(['rotation']), type(term), default(0), help('Rotiert das gasket um einen bestimmten Winkel (in Rad).')]
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
			, member(rotation(Rotation), Optionen)
			
			% Postionale Argumente Parsen
			, PositionaleArgumente = [_, RadiusText1, RadiusText2, RadiusText3|FarbpaletteTexte]
			, text_to_string(RadiusText1, RadiusString1)
			, maplist(text_to_string, [RadiusText1, RadiusText2, RadiusText3], [RadiusString1, RadiusString2, RadiusString3])
			, maplist(number_string, [Radius1, Radius2, Radius3], [RadiusString1, RadiusString2, RadiusString3])
			
			% Farbpalette bestimmen
			, (FarbpaletteTexte = [], Farbpalette = [255/255/255]; maplist(text_to_string, FarbpaletteTexte, FarbpaletteStrings), maplist(term_string, Farbpalette, FarbpaletteStrings))
			
			% Färbealgorithmus bestimmen
			, (member(farbe_zufall(true), Optionen), FarbpaletteTexte = [], Farbmodus = zufall; true)
			, (member(farbe_zufall(true), Optionen), FarbpaletteTexte \= [], Farbmodus = zufall_palette; true)
			, (member(farbe_radius_interpolierend(true), Optionen), Farbmodus = radius_interpolierend; true)
			, (member(farbe_generation(true), Optionen), Farbmodus = generation; true)
			, (member(farbe_zufall_palette_interpolierend(true), Optionen), Farbmodus = zufall_palette_interpolierend; true)
			, (member(farbe_generation_interpolierend(true), Optionen), Farbmodus = generation_interpolierend; true)
			, (member(nested_scaled(true), Optionen), Nesting = nested_gasket_scaled; true)
			, (member(nested_random(true), Optionen), Nesting = nested_gasket_random; true)
			, (member(nested_relfected(true), Optionen), Nesting = nested_gasket_reflected; true)
			, (Nesting = nested_gasket_off; true)
			, (Farbmodus = generation; true)
			
			% Generierung aufrufen
			, !
			, gasket(
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
				, GasketFarbe
				, HintergrundFarbe
				, Farbmodus
				, Farbpalette
			)
		)
	)
.
