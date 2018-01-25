(*
	Author: Jacob Pihl
	Date: 07 Oct 2017
	Description: Homework 1 for Programming Languages
*)

(* Helper functions *)
let is_even n =
	n mod 2 = 0

(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n ->
match n with
	0 -> 1
	|1 -> b
	|_ -> if (is_even n) then
			(fastexpt b (n/2))*(fastexpt b (n/2))
		else 
			b*(fastexpt b (n-1))