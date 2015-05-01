(*Geometric search algorithm.  The main function
  geo_search will take a float vectorand a linear program
  and return an int vector option if any neighbors satisfy the 
  linear program*)

open main

let MAX = 256;;

(* Rounds a float to an int, careful to avoid floating point errors *)
let round (f: float):int
    if (f -. (floor f))< .5 then int_of_float f else int_of_float f + 1;;

(* Adds, and subtracts two vectors *)
let subtract (v1: int vector) -> (v2: int vector): int vector =
  Array.mapi (fun x i -> x - (Array.get v2 1)) v1;;

let add (v1: int vector) -> (v2: int vector): int vector =
  Array.mapi (fun x i -> x + (Array.get v2 1)) v1;;

(* Gets a list of neighbors by shifting to unit cube, where neighbors can be found with XOR *)
let neighbors (v: int vector) (interior: float vector): int vector list =
  let int_part = Array.map int_of_float interior in
  let new_v = subtract v int_part in
  let neighs = Array.create (Array.length new_v) new_v in
  Array.iteri (fun i pt -> Array.set pt i (lxor 1 (Array.get pt i))) neighs;
  let list_neighs = Array.to_list neighs in
  List.map (add int_part) unshifted_neighs;;

(* Distance function for distance between corner and interior point *)
let distance (p1: float vector) -> (p2: int vector) : float =
  sqrt (Array.fold_left ( + ) 0 (Array.mapi (fun x i -> (x -. (float_of_int (Array.get p2 i)))**2) p1));;

(* This function checks if an int vector is a solution to a linear program *)
(* TODO should it be int or float? Damn Ocaml for making this an issue *)
let check (lp: linProg)->(sol: int vector): bool =
  let (_, constraint_matrix) = lp in
  let ((vars,cons),mat) = constraint_matrix in
  if  (vars-1) <> Array.length sol then false else
    let val_at_cons = Array.init cons (fun i ->
      Array.fold ( + ) 0 
	(Array.init (vars-1) (fun j -> 
	  (Array.get (Array.get mat j) i)*(Array.get sol j)))) in
    let constraint_limits = Array.get mat (vars-1) in
    let satisfies = Array.mapi (fun i x -> x < Array.get constraint_limits i) val_at_cons in
    Array.fold_left (fun b1 b2 -> (b1 and b2)) true satisfies;;

(* Searchs the points for the first solution *)
(* Checks a point, if not a solution, adds its neighbors to frontier, a sorted list of ones to visit. *)
(* Recursively searches frontier *)
let rec search (lp: linProg)->(point: float vector)->(test_point: int vector)-> 
  (visited: int vector list)-> (frontier: (int vector * float) list)->(max: int): int vector option = 
  if max < 0 then None else
    if check lp test_point then Some test_point else
      let new_points = List.filter (fun p -> !(List.mem visited p)) (neighbors test_point point) in
      let new_points_dist = List.sort (fun (p1,v1) (p2,v2) -> if v1 < v2 then -1 else 1) (List.map (fun p -> (p, distance point p))) in
      let new_front = List.merge (fun (p1,v1) (p2,v2) -> if v1 < v2 then -1 else 1) frontier news_dist in
      if new_front = [] then None else
	search lp point (List.hd new_front) (test_point::visited) (List.tl new_front) (max-1);;

(* Call this function, it performs geometric search *)
let geo_search (lp: linProg)->(approx_sol: float vector) : int vector option =
  let start_point = Array.map round approx_sol in
  search lp approx_sol start_point [] [] MAX;;
   


  
