open Core.Std
open Matrix

type 'a Vector = 'a array (* Might model after stacks *)
type linProg = ('a Vector * matrix)

type matrix = (int * int) * (elt array array)

(* First parameter is our input, second is the current matrix.
  To use this function, pass in an empty matrix as the second argument *)

let rec io ( ((n,p),m) : matrix ) ( ((n1,p1),m1) : matrix) : linProg =
  (
    (match (get_row ((n,p),m) 1) with
  | (_, array) -> array),
  (removeTopRow ((n,p),m) (empty (n-1) p) 2)
  )

(* removeTopRow m (empty (n-1) p) 2 *)
let rec removeTopRow ( (n,p),m) : matrix ) (newM : matrix) (counter : int) :
  matrix =
  if counter <= n then (
    set_row newM counter (match (get_row ((n,p),m) counter) with
                 | (_, a) -> a) ;
    removeTopRow ((n,p),m) (counter+1) )
  else
    newM
