# CapsLock
Repository for CS51 Final Project

User Input Table -> Linear Program -> (Cinput -> Simplex -> Coutput) -> Vector ->

  Then we have to decide whether we do Branch and Bound or Geometric

If Branch and Bound -> produce another Linear Program then see above

If Geometric ->  Int Vector

Example of User Input Table:
1	1	0             -> This first line is a vector
4	3	2	17          -> These lines combined will be a matrix
-2	0	8	4
0	2	5	-6

Example of a Linear Program Object
tuple (obj function of type VECTOR, constraint of type MATRIX)

User Input
Black Box
What to do with Vector
Someone to do geometric search

TO RUN
Type make with the directory open to compile the code. Then run the program by typing in ./simplex
