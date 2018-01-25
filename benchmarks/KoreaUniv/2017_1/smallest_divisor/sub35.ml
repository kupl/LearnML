(*
	Author: Jacob Pihl
	Date: 07 Oct 2017
	Description: Homework 1 for Programming Languages
*)

(* Helper functions *)
let is_even n =
	n mod 2 = 0
(* problem 2*)

let rec smallest_divisor : int -> int
= fun n ->
if (is_even n) then 	(* Number is even *)
	2
else
	let rec loop n d =
		if n mod d = 0 then d 	(* Number divisible by d *)
		else if d*d > n then n 	(* Number is prime *)
		else loop n (d+2) 		(* Do the ugly loop *)
	in loop n 3