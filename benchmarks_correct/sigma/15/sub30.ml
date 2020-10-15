(* 생명과학부/2011-10915/신지민 Homework 1-2 *)


let rec sigma f a b  =
 	if(a==b) then f a
	else if(a>b) then 0
	else sigma f a (b-1) + sigma f b b

(*
let f : int -> int = fun x->x+1


let x : int  = sigma (7,7,f)
let _= print_endline(string_of_int x) 
	
let x : int = sigma (10,7,f)
let _= print_endline(string_of_int x)
*)
