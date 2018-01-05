(* 2011-10915 / 생명과학부 / 신지민 / Homework 1-3 *)

let rec iter = fun(n,f) a ->
	if(n==0) then a
	else f (iter(n-1,f) a)



let k = iter(1, function x -> x) 1.323413

let _= print_endline(string_of_int k)


