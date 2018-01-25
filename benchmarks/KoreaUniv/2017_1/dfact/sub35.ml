(*
	Author: Jacob Pihl
	Date: 07 Oct 2017
	Description: Homework 1 for Programming Languages
*)

(* Helper functions *)
let is_even n =
	n mod 2 = 0

(* problem 4*)
let rec product : (int -> int) -> int -> int -> int
= fun f a b ->
if a = b then f a
else (product f (a+1) b)*(f a)

(* problem 5*)
let rec dfact : int -> int
= fun n ->
if is_even n then (fastexpt 2 (n/2)) * (product (fun x->x) 1 (n/2))
else (product (fun x->x) 1 n) / (dfact (n-1))