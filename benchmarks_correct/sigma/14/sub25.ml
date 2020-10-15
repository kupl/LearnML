(* College of Liberal Studies 2010-13342 Kim Ye Jung *)
(* 2014.2 Programming Languages Homework 1 - 1 *)
let rec sigma f a b =
	if a > b then 0
	else sigma f (a+1) b + f a
