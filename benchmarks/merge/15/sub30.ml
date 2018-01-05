(* 생명과학부/2011-10915/신지민 Homework 1-1 *)


let rec sigma : int * int * (int->int) -> int = fun (a,b,f) ->
 	if(a==b) then f a
	else sigma (a,b-1,f) + sigma (b,b,f)

(*
let x : int  = sigma (7,10,f)
let _= print_endline(string_of_int x) 
*)
