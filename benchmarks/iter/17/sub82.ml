(*
	CSE / 2013-11426 / Im DongYeop
	Homework 1 : Exercise 3
*)

let compose((f: 'a -> 'a), (g: 'a -> 'a)): 'a -> 'a = fun x -> f(g(x))

let rec iter((n: int), (f: 'a -> 'a)): 'a -> 'a =
	match n with
	| 0 -> fun x -> x
	| 1 -> f
	| 2 -> compose(f, f)
	| _ -> compose(f, iter(n - 1, f))

