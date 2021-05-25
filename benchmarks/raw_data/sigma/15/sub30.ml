(* 생명과학부/2011-10915/신지민 Homework 1-2 *)


let rec sigma : int * int * (int->int) -> int = fun (a,b,f) ->
 	if(a==b) then f a
	else if(a>b) then 0
	else sigma (a,b-1,f) + sigma (b,b,f)

(*
let f : int -> int = fun x->x+1


let x : int  = sigma (7,7,f)
let _= print_endline(string_of_int x) 
	
let x : int = sigma (10,7,f)
let _= print_endline(string_of_int x)
*)
