#use "topfind";;
#require "lwt";;


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

(* PLOTTING *)

let rec gen_xy l f =
match l with
| [] -> []
| h::t -> [(h, (f h))]@gen_xy t f
;;

let rec desmos_xy pts =
match pts with
| [] -> nan
| (x,y)::t -> (Printf.printf "(%4f,%4f)," x y); (desmos_xy t)
;;

(* Calc. *)

let grad f x = ((f (x +. 0.00000001)) -. (f x)) /. ((x +. 0.00000001) -. x)
;;



 end