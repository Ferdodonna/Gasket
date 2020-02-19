REM Blau nach Rot nach Gelb nach Schwarz:
gasket -o 'example1.svg' -r 1 --radius-coloring 1000 2000 1500 30/30/190 200/70/50 220/190/30 0/0/0

REM Generation-Coloring Rot nach Schwarz nach Weiß
gasket -o 'example2.svg' -r 1 --generation-coloring 2000 1500 1000 190/0/0 0/0/0 255/255/255

REM Radius-colring Rot nach Schwarz nach Weiß (Spezialfall)
gasket -o 'example3.svg' -r 1 --radius-coloring 2000 70 1000 190/0/0 0/0/0 255/255/255
