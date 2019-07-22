(*
 * Dept of Physics Education
 * 2012-12666 Choi Jaehyeok
 * Homework 1, Problem 2
 *)

let rec sigma: (int * int * (int -> int)) -> int = fun (a, b, f) ->
	if (a > b) then 0
	else (f a) + sigma (a+1,b,f)
