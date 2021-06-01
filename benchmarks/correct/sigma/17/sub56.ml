(*
 * Dept of Physics Education
 * 2012-12666 Choi Jaehyeok
 * Homework 1, Problem 2
 *)

let rec sigma f a b =
	if (a > b) then 0
	else (f a) + sigma f (a+1) b
