open Core.Std
open Matrix

type 'a vector = 'a array
type linProg = 'a vector * matrix

(* First parameter is our input, second is the current matrix.
  To use this function, pass in an empty matrix as the second argument *)

let rec io ( ((n,p),m) : matrix ) : linProg =
  (
    (match (get_row ((n,p),m) 1) with
    | (_, array) -> array),
    (removeTopRow ((n,p),m) (empty (n-1) p) 2)
  )

(* Returns a matrix without the top row.
  Ex: removeTopRow m (empty (n-1) p) 2 *)
let rec removeTopRow ( ((n,p),m) : matrix ) (newM : matrix) (counter : int) :
  matrix =
  if counter <= n then (
    set_row newM counter (match (get_row ((n,p),m) counter) with
                 | (_, a) -> a) ;
    removeTopRow ((n,p),m) (counter+1) )
  else
    newM

(* prep : LinearProgram -> Cinput*)
let prep (prog : linProg) =
()

(* simplex : Cinput -> Coutput*)
let simplex =
()

(* unprep : Coutput -> float vector*)
let unprep : float vector =
()

(* branch_and_bound : float vector -> LinearProgram*)
let branch_and_bound (vec : float vector) : linProg =
()

(* geo_search : float vector -> LinearProgram -> int vector*)
let geo_search (vec : float vector) (prog : linProg) : int vector =
()
