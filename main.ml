open Core.Std
open Matrix

type 'a Vector = 'a array (* Might model after stacks *)
type linProg = ('a Vector * matrix)

type matrix = (int * int) * (elt array array)
(array, newM)

let io ( (n,p),m) : matrix ) : linProg =
  let newM = empty x y in
  match (get_row m 1) with
  |(int, array) -> array in
