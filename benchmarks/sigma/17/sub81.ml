(*
		CSE / 2013-11426 / Im DongYeop
		Homework 1 : Exercise 2
*)

let rec sigma((a: int), (b: int), (f: int -> int)): int =
	if (a > b)
		then 0
	else if(a == b)
		then f(a)
	else
		f(a) + sigma(a + 1, b, f)
