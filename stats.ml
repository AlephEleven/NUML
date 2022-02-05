module NUML = struct

(* STATS *)

let rand_float bound_a bound_b = Random.float (bound_b-.bound_a+. 1.) +. bound_a
;;

let rec rand_data n bound_a bound_b =
match n with
| 0 -> []
| _ -> (rand_float bound_a bound_b)::(rand_data (n-1) bound_a bound_b)
;;

let rec sum l =
match l with
| [] -> 0.
| h::t -> h+.sum t
;;

let mean (l : float list)  = (sum l) /. float_of_int (List.length l)
;;

let square x = x*.x
;;

let rec var' l mu =
match l with
| [] -> 0.
| h::t -> square (h -. mu) +. (var' t mu)
;;

let rec var l = (1. /. (float_of_int (List.length l))) *. var' l (mean l)
;;

let stdev l = square (var l)
;;

let norm_dist stdev mu x = (1.)/. (Float.sqrt (2. *. Float.pi)) *. Float.exp ( (-0.5) *. (square ((x-.mu) /. stdev)) )
;;

let st_norm_dist x = norm_dist 1. 0. x
;;

(* FUNCTION/SAMPLE POINTS IN RANGE *)

let rec linspace' start step n =
match n with
| 0 -> []
| _ -> (linspace' start step (n-1))@[start +. ((float_of_int n) *. step)]
;;

let linspace start stop n = [start] @ (linspace' start ((stop-.start)/.((float_of_int n))) n)
;;

let rec map l f =
match l with
| [] -> []
| h::t -> (f h)::map t f
;;

let indx l n = List.nth l n
;;

let indx2d l n_x n_y = List.nth (List.nth l n_x) n_y
;;

(* GRAPHICS *)

let rec gen_row n =
match n with
| 0 -> []
| _ -> [" "]@gen_row (n-1)
;;

let rec gen_sq x y =
match x with
| 0 -> []
| _ -> (gen_row y)::gen_sq (x-1) y
;;

let rec set_row' l indx key cur =
match l with
| [] -> []
| h::t when indx=cur -> key::set_row' t indx key (cur+1)
| h::t -> h::set_row' t indx key (cur+1)
;;

let set_row l indx key = set_row' l indx key 0
;;

let rec set_sq' l indx_x indx_y key cur_x =
match l with
| [] -> []
| h::t when cur_x = indx_x -> (set_row h indx_y key)::set_sq' t indx_x indx_y key (cur_x+1)
| h::t -> h::set_sq' t indx_x indx_y key (cur_x+1)
;;

let set_sq l indx_x indx_y key = set_sq' l indx_x indx_y key 0
;;

let is_btwn a b key = if key > a && key <= b then true else false
;;


let rec linbtwn' linsp key indx =
match linsp with
| [] -> -1
| x::y::t when (is_btwn x y key) -> indx
| h::t -> linbtwn' t key (indx+1)
;;

let linbtwn linsp key = linbtwn' linsp key 0
;;

(* 
Example: Plotting 0.5 on a line between -1 and 1

        Graphic [0 - 10], Check where 0.5 is between in linspace -1 to 1 w/ 10 cuts, set a * there
set_row (gen_row 10) (linbtwn (linspace (-1.) 1. 10) 0.5) "*"
;;
 *)

 end
