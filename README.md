
This project is all about those guys:

See below examples for tutorial on how to create your own drawings! (Todo: Formatting + Command line tutorial)

![alt text](https://github.com/Ferdodonna/Gasket/blob/master/examples/example11.PNG)
![alt text](https://github.com/Ferdodonna/Gasket/blob/master/examples/example7.PNG)
![alt text](https://github.com/Ferdodonna/Gasket/blob/master/examples/example1.PNG)
![alt text](https://github.com/Ferdodonna/Gasket/blob/master/examples/example9.PNG)
![alt text](https://github.com/Ferdodonna/Gasket/blob/master/examples/example3.PNG)
![alt text](https://github.com/Ferdodonna/Gasket/blob/master/examples/example4.PNG)
![alt text](https://github.com/Ferdodonna/Gasket/blob/master/examples/example8.PNG)
![alt text](https://github.com/Ferdodonna/Gasket/blob/master/examples/example12.PNG)

This project implements a script in Prolog that spits out [Apollian Gaskets](https://en.wikipedia.org/wiki/Apollonian_gasket). There are two ways to use it - either over command line or the SWI Prolog environment -  both require you to install [SWI Prolog](https://www.swi-prolog.org).

Using the provided SWI Prolog environment, you simply consult the gasket.pl and then execute the gasket/14 predicate. The gasket predicate works as follows:



gasket( 
   
    Radius1, Radius2, Radius3
   
   , Rotation
   
   , Output path for svg file
   
   , Amount of generations to draw at least
   
   , Amount of circles to draw at least
   
   , Smallest value a circle radius can have
   
   , nesting algorithm
   
   , generationfilter via clpfd domains
   
   , color of biggest circle
   
   , background color
   
   , coloring algorithm
   
   , colors for coloring algorithm

)

      
So given the input 

![alt text](https://github.com/Ferdodonna/Gasket/blob/master/examples/example_input.PNG)

the script would draw the pattern from three starting circles all with a radius of 100, go at least to generation 5/circle amount 2200/radius size 0.1, use no nesting algorithm, draw all generations but generation 1, make the biggest circle black and the background white, use the radius interpolation algorithm for coloring given the RGB colors (123,293,23) and (18,255,91).

# Possible inputs for each argument

Three inital radii: everything greater than 0

Rotation: 0 - 2π

Output path: needs an .svg ending!

Amount of generations to draw at least: a natural number, if this limiter should be ignored input -1

Amount of circles to draw at least: a natural number, if this limiter should be ignored input -1

Smallest value a circle radius can have: a floating point number, if this limiter should be ignored input -1

Nesting Algorithm: A nesting algorithm describes a way to draw additional circles into already drawn circles. We currently have three 

options:
          
          1. nested_gasket_off => no nesting algorithm
          
          2. nested_gasket_scaled => nesting algorithm that tries to copy the original gasket pattern into every circle, affected by limiters
          
          3. nested_gasket_random => nesting algorithm that randomly draws apollian gaskets into circles, affected by limiters

Generationfilter: Draws all generations in the given domain, to draw all generations input inf..sup

Color of biggest circle: RGB value in the R/G/B format

Background color: RGB value in the R/G/B format

Coloring algorithm: Currently we have six coloring algorithms:


        1. zufall => random coloring using all possible colors

        2. zufall_palette => random coloring using the given colors

        3. zufall_palette_interpolierend => random coloring interpolating between the given colors

        4. generation => given a list of colors C of the length L and the generation N, N being a natural number, the generation receives the color ((N mod L) + 1)th color in the list of colors
          
        5. generation_interpolierend => we interpolate between the given colors based on the generation
          
        6. radius_interpolierend => we interpolate between the given colors based on radius

Colors: a list of colors, each color in the R/G/B format



