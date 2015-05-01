(*Geometric search algorithm.  The main function
  geo_search will take a float vectorand a linear program
  and return an int vector option if any neighbors satisfy the
  linear program*)

(*I didn't have access to your vecotr type, so I built my own, should be easy to swap it later*)
type 'a vector = 'a list;;

let fun get_nth (n: int)->(v: 'a vector): 'a =
  nth v n;;


let fun project (dim: int)-> (v: 'a vector): 'a vector =
  match v with
  |[] -> assert(false)
  |hd::tl -> if dim = 0 then tl else hd::(project (dim-1) tl);;

let fun reverse_project (dim: int) -> (value: 'a) -> (v: 'a vector): 'a vector=
` if dim = 0 then value::v else if d = [] then assert false else let (hd::tl)=v in hd::(reverse_project (dim-1) value tl);;

let rec order_pair (v: 'a vector): 'a*int vector =
  match v with
  |[] -> []
  |x::[] -> [(x,0)]
  |hd::tl -> let ((y,n)::tl2) = (order_pair tl) in (hd,n+1)::(y,n)::tl2;;

let near_int (f: float): int =
  if  (abs (f -. (ceil f)))< (abs (f-.(floor f))) then (int (ceil f)) else (int (floor f));;

let far_int (f: float): int =
  if  (abs (f -. (ceil f)))>= (abs (f-.(floor f))) then (int (ceil f)) else (int (floor f));;

let nearness (f: float): float = min (abs (f -. (ceil f))) (abs (f -. (floor f)));;
(*sorts tuples based on which dimessnion corresponds to closeness to an edge*)
let pair_sort (p1:float*int)->(p2:float*int): int =
  let ((f1,_),(f2,_))= (p1,p2) in
  let (r1,r2) = (nearness f1, nearness f2)in
  if r2 >= r1 then -1 else 1;;
(*  gets the points nearest to a list in order FLAWED*)
let rec nearest_list (v: float*int vector) -> (max_len: int) : int vector list=
  let (num, place) = List.hd v in
  let proj_nearest = nearest_list (project 0 v) max_len in
  List.append (reverse_project 0 (near_int num, place) proj_nearest) (reverse_project 0 (far_int num, place) proj_nearest)

(*Given an int list and another list, performs the operations on the other list that would sort the int list, want fast as possibble time in length of the other list*)
(*NOT WRITTEN*)
let depermute (perm: int list)-> (permed: 'a list): 'a list=
  permed;;

(*Given a vector produces a list of corners ordered by nearness, DOES NOT YET WORK*)
let get_list (v: float vector): float*int vector =
  let ord_v = order_pair v in
  let  permuted_points_list =  nearest_list (List.sort pair_sort ord_v) in
  let perm = (List.map ~f:(fun (x,y) -> y) (List.hd permuted_points_list))in
  let ordered_points_list = (List.map ~f:(depermute perm) permuted_points_list) in
  let points_list = List.map ~f:(List.map ~f:(fun (x,y) ->  x)) ordered_points_list in
  points_list;;

(*NOT A REAL FUNCTION, just used so I can get this to type check*)
let check (lp: LinearProgram)->(sol: int vector): bool =
  true;;

(*searchs a list of points for the first solution*)
let rec search (sols: int vector list) -> (lp: LinearProgram) : int vector option =
  match sols with
  |[] -> None
  |x::xs -> if check lp x then Some x else search xs lp;;

(*Call this function, it performs geometric search*)
let geo_search (lp: LinearProgram)->(approx_sol: float vector) : int vector option =
  let corners = get_points approx_sol in
  search corners lp ;;