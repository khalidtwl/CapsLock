open Elts
open MatrixI
open SimplexI
open Matrix
open BranchandBound
open GeoSearch

type 'a vector = 'a array
type linProg = (EltMatrix.elt vector) * EltMatrix.matrix

(* Returns a matrix without the top row.
  Ex: removeTopRow m (empty (n-1) p) 2 *)
let rec removeTopRow (oldM : EltMatrix.matrix ) (newM : EltMatrix.matrix)
  (counter : int) : EltMatrix.matrix =
  let n = (match (EltMatrix.get_dimensions oldM) with
          | (rows, _) -> rows) in
  (if counter <= n then (
    EltMatrix.set_row newM (counter-1)
    (match (EltMatrix.get_row oldM counter) with
      | (_, a) -> a );
    removeTopRow oldM newM (counter+1) )
  else
    newM)

(* First parameter is our input, second is the current matrix.
  To use this function, pass in an empty matrix as the second argument *)
let rec io ( matx : EltMatrix.matrix ) : linProg =
  (
    (match (EltMatrix.get_row matx 1) with
    | (_, array) -> array),
    (let (n,p) = (EltMatrix.get_dimensions matx) in
             removeTopRow matx (EltMatrix.empty (n-1) p) 2)
  )

let rec printArray (arr : 'a array) : unit =
  Array.iter (fun x -> (Elts.print x)) arr


(* Prints a solution *)
let print_solution (e,p) : unit =
  let _ = print_string "Solved!\n\nOptimal value: " in
  let _ = Elts.print e in
  let _ = print_string "\n\nAchieved at the point: " in
  let _ = Simplex.print_point p in
  print_string "\n\n"

let solve_simplex (lp : linProg) : float vector option =
  let (a, b) = lp in
  let neg_one = Elts.subtract Elts.zero Elts.one in
  let obj_lst = neg_one::(Array.to_list a) in
  (* Maybe you meant to use EltMatrix.map here instead? *)
  let b_lst =
    let (height, _) = EltMatrix.get_dimensions b in
    let rec extracted n lst: 'a list list =
      if n >= height then lst
      else
        let n' = n + 1 in
        let (_, row) = EltMatrix.get_row b n' in
          extracted n' (Array.to_list row::lst) in
    List.rev (extracted 0 []) in
  let cons_lsts = List.map (fun x -> Elts.zero::x) b_lst in

  match Simplex.load_matrix (EltMatrix.from_list (obj_lst::cons_lsts)) with
    | None -> (print_string
        "\nThis system has no feasable solution.\n"); None
    | Some sys ->
      let _ = print_string "\nSolving your system....\n\n" in
      match Simplex.solve sys with
      | None -> (print_string "This system is unbounded.
        You can increase/decrease it as you please!\n"); None
      | Some solution -> print_solution solution;
        (let (e,p) = solution in
          Some (Array.of_list (Simplex.point_to_list p)))
	  

(* Master function that takes a matrix and solves it with our algorithm *)
let solve (mat: EltMatrix.matrix): unit =

  let lp = io mat in
   let first_approx = solve_simplex lp in
    match first_approx with
    |None -> Printf.printf "No Solution Found"
    |Some approx ->
      match branch_and_bound approx lp with
      |None -> Printf.printf "No Solution Found"
      |Some solution -> Printf.printf "Solution found:  "; printArray (Array.map int_to_elt solution)

let sampleMatrix = EltMatrix.from_string "5,4,3,0|2,3,1,5|4,1,2,11|3,4,2,8"
let sample2 = EltMatrix.from_string "5,4,3,0|2,3,1,5|-2,-3,-1,-6|3,4,2,8"
let sample3 = EltMatrix.from_string "1,1,0|1,2,4|2,1,4"


let () = match (io sample3) with
          | (vect, mat) -> printArray vect; EltMatrix.print mat

let () = solve sample3

(*let lp = io sample2
let _ = solve_simplex lp*)
