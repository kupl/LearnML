(*
 * Dept of Physics Education
 * 2012-12666 Choi Jaehyeok
 * Homework 1, Problem 3
 *)

let rec iter: (int * ('a -> 'a)) -> 'a -> 'a = fun (n, f) v ->
	if (n <= 0) then v
	else iter (n - 1, f) (f v)
