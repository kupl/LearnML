(* College of Liberal Studies 2010-13342 Kim Ye Jung *)
(* 2014.2 Programming Languages Homework 1 - 1 *)
let rec sigma : int * int * (int -> int) -> int =
	fun(a, b, f) ->
	if a > b then 0
	else sigma(a+1, b, f) + f a
