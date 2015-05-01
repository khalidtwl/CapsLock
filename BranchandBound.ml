(* This is where all the code for branch and bound livves.  The goal
   is solve an integer program by calling simplex *)
open GeoSearch
open Elts
open MatrixI
open SimplexI
(*open Core.Std*)
open Matrix


(* Gives a list of fixes for a vector, sorted by which is the smallest shift. *)
let branch_direction_priority_list (approx: float vector): (int*int) list =
  let coordinated = Array.mapi (fun i fl -> (i,fl)) approx in
  let unsorted = Array.fold_left (fun lst (i,fl) ->
    (i, int_of_float fl)::(i,int_of_float fl +1)::lst) [] coordinated in
  List.sort (fun (i1, v1) (i2,v2) ->
    let (d1,d2) = (abs_float((Array.get approx i1) -. (float_of_int v1)), abs_float((Array.get approx i2) -.(float_of_int v2))) in
    if d1<d2 then -1 else 1) unsorted;;

let fix_vect (i: int) (vect: 'a vector): 'a vector =
  Array.append (Array.sub vect 0 i) (Array.sub vect (i+1) (Array.length vect));;

let int_to_elt (i: int): EltMatrix.elt =
  Elts.from_string (Num.string_of_num (Num.num_of_int i))

(*fixes a variable to a value in a linear program *)
(*CHECK ROW COLUMN VS COLUMN ROW*)
let fix (index: int) (value: int) (lp: linProg): linProg =
  let (obj_funct, constraint_matrix) = lp in
  let new_obj_funct = fix_vect index obj_funct in
  let (rows, cols) = EltMatrix.get_dimensions constraint_matrix in
  let new_lp = EltMatrix.empty rows cols in
  let _ = EltMatrix.iteri
    (fun row col e -> if col < index
      then EltMatrix.set_elt new_lp (row,col) e
      else if col < cols
      then EltMatrix.set_elt new_lp (row, col-1) e
      else EltMatrix.set_elt new_lp (row, col)
      (Elts.subtract e (Elts.multiply (EltMatrix.get_elt constraint_matrix
        (row, index)) (int_to_elt value))))
      constraint_matrix;
 in (new_obj_funct, new_lp);;


(* This function takes a solution, a LP and a list of fixes, tuples that say fix variable i to v*)
(* It checks a geo_search then implements branch and bound in order of priority*)
let rec branch_and_bound_call (approx_sol: float vector) (lp: linProg) (fixes_list: (int*int) list): int vector option =
  match geo_search lp approx_sol with
  |Some sol -> Some sol
  |None -> match fixes_list with
    |[] -> None
    |(i,v)::tl -> match branch_and_bound_call (fix_vect i approx_sol) (fix i v lp) tl with
      |Some sol -> Some sol
      |None -> match branch_and_bound_call approx_sol lp tl with
	|Some sol -> Some sol
	|None -> None;;

(* call this function to run a branch  and bound *)
let branch_and_bound (approx_sol: float vector) (lp: linProg): int vector option =
  branch_and_bound_call approx_sol lp (branch_direction_priority_list approx_sol)
