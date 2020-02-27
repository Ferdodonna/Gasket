REM Blau nach Rot nach Gelb nach Schwarz:
gasket -o 'example1.svg' -r 1 --radius-coloring 1000 2000 1500 30/30/190 200/70/50 220/190/30 0/0/0

REM Generation-Coloring Rot nach Schwarz nach Weiß
gasket -o 'example2.svg' -r 1 --generation-coloring 2000 1500 1000 190/0/0 0/0/0 255/255/255

REM Radius-colring Rot nach Schwarz nach Weiß (Spezialfall)
gasket -o 'example3.svg' -r 1 --radius-coloring 2000 70 1000 190/0/0 0/0/0 255/255/255

REM Nested Gasket Scharz/Weiß mit emergenten Dreiecken
gasket -o 'example4.svg' -r 1 --radius-coloring --scaled-nesting 200 200 200 0/0/0 100/100/100 140/140/140 255/255/255 0/0/0 255/255/255 255/255/255 200/200/200

REM Nested Gasket Scharz/Grau mit emergenten Dreiecken
gasket -o 'example5.svg' -r 1 --radius-coloring --rotation 1.1 --scaled-nesting 200 200 200 0/0/0 100/100/100 140/140/140 100/100/100 0/0/0 255/255/255 200/200/200

REM Nested Gasket Blau/Schwarz mit emergenten Dreiecken
gasket -o 'example6.svg' -r 1 --radius-coloring --rotation 1.9 --scaled-nesting 200 200 200 0/0/0 100/100/100 255/255/255 100/100/200 0/0/0 255/255/255 200/200/200

REM Gasket mit Eulen!!!
gasket -o 'example7.svg' -r 0.6 --generation-coloring --scaled-nesting 200 200 200 255/0/0 0/0/255

REM Rote Landkarte des Mondes mit Hieroglyphen und einer seltsamen Linie
gasket -o 'example8.svg' -r 1 --generation-coloring --scaled-nesting 500 200 100 255/0/0 0/0/0

REM Nested Gasket Rot/Weiß
gasket -o 'example9.svg' -r 1 --generation-coloring --gasket 255/255/255 -f 5..sup --rotation 4.712389 --scaled-nesting 500 200 100 255/0/0 255/255/255

REM Nested Gasket: Abstract Art (Sparse in a Red-Blue Theme)
gasket -o 'example10.svg' -g 5 --radius-coloring --gasket 0/0/50 -f 1\/5 --rotation 1.6 --scaled-nesting 500 200 80 0/0/255 255/0/0 255/255/255