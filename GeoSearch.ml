(*Geometric search algorithm.  The main function
  geo_search will take a float vectorand a linear program
  and return an int vector option if any neighbors satisfy the
  linear program*)

open Elts
open MatrixI
open SimplexI
(*open Core.Std*)
open Matrix

type 'a vector = 'a array
type linProg = EltMatrix.elt vector * EltMatrix.matrix

let max : int = 256;;


(* Rounds a float to an int, careful to avoid floating point errors *)
let round (f: float):int=
    if (f -. (floor f))< 0.5 then int_of_float f else int_of_float f + 1;;

(* Adds, and subtracts two vectors *)
let subtract (v1: int vector) (v2: int vector): int vector =
  Array.mapi (fun x i -> x - (Array.get v2 1)) v1;;

let add (v1: int vector) (v2: int vector): int vector =
  Array.mapi (fun x i -> x + (Array.get v2 1)) v1;;

(* Gets a list of neighbors by shifting to unit cube, where neighbors can be found with XOR *)
let neighbors (v: int vector) (interior: float vector): int vector list =
  let int_part = Array.map int_of_float interior in
  let new_v = subtract v int_part in
  let neighs = Array.make (Array.length new_v) new_v in
  Array.iteri (fun i pt -> Array.set pt i ( 1 lxor  (Array.get pt i))) neighs;
  let list_neighs = Array.to_list neighs in
  List.map (add int_part) list_neighs;;

(* Distance function for distance between corner and interior point *)
let distance (p1: float vector) (p2: int vector) : float =
  sqrt (Array.fold_left ( +. ) 0.0 (Array.mapi (fun i x -> (x -. (float_of_int (Array.get p2 i)))**2.0) p1));;


let int_to_elt (i: int): EltMatrix.elt =
  Elts.from_string (Num.string_of_num (Num.num_of_int i))

(* This function checks if an int vector is a solution to a linear program *)
(* TODO should it be int or float? Damn Ocaml for making this an issue *)
let check (lp: linProg) (sol_raw: int vector): bool =
  let sol_raw_num = Array.map  int_to_elt sol_raw in
  let sol = EltMatrix.from_list [((Array.to_list sol_raw_num)@[int_to_elt 0])] in
  let (_, constraint_matrix) = lp in
  let (cols,rows) = EltMatrix.get_dimensions constraint_matrix in
  let values_at_constraints = EltMatrix.mult constraint_matrix sol in
  let (_, values_at_constraints_vector) = EltMatrix.get_column values_at_constraints 1 in
  let (_, constraint_limits) = EltMatrix.get_column constraint_matrix cols in
  let satisfies = Array.mapi (fun i x -> x < Array.get constraint_limits i) values_at_constraints_vector in
  Array.fold_left (fun b1 b2 -> (b1 && b2)) true satisfies;;

(* Searchs the points for the first solution *)
(* Checks a point, if not a solution, adds its neighbors to frontier, a sorted list of ones to visit. *)
(* Recursively searches frontier *)
let rec search (lp: linProg)(point: float vector)(test_point: int vector)
  (visited: int vector list)(frontier: (int vector * float) list)(max: int): int vector option =
  if max < 0 then None else
    if check lp test_point then Some test_point else
      let new_points = List.filter (fun p ->  not (List.mem p visited)) (neighbors test_point point) in
      let new_points_dist = List.sort (fun (p1,v1) (p2,v2) -> if v1 < v2 then -1 else 1)
	(List.map (fun p -> (p, distance point p)) new_points) in
      let new_front = List.merge (fun (p1,v1) (p2,v2) -> if v1 < v2 then -1 else 1) frontier new_points_dist in
      if new_front = [] then None else
	let (new_test_point,_) = List.hd new_front in
	search lp point new_test_point (test_point::visited) (List.tl new_front) (max-1);;

(* Call this function, it performs geometric search *)
(* TODO make max a variable *)
let geo_search (lp: linProg)(approx_sol: float vector) : int vector option =

  let start_point = Array.map round approx_sol in
  search lp approx_sol start_point [] [] 256;;


(* Tests *)(*
let list_equality (l1: 'a list) (l2: 'a list) -> bool = true;;



let point = [|2.2; 1.9; 1.7|] in
let c1 =  [|2;1;1|] in
let c2 = [|3;2;2|] in
let c3 = [|2;2;2|] in
assert(list_equality (neighbors c1)[ [|3;1;1|], [|2;2;1|], [|2;1;2|] ]);
assert(list_equality (neighbors c2)[ [|2;2;2|], [|3;2;1|], [|3;1;2|] ]);
assert(list_equality (neighbors c1)[ [|3;2;2|], [|2;2;1|], [|2;1;2|] ]);


let point = *)
