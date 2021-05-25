(*snucse, 200611810, exercise01*)

exception Error of int*int

let rec sigma (a,b,f) =
	if a=b then f a
	else if a<b then f a + sigma((a+1),b,f)
	else raise (Error(a,b))

(*
let f x = x+1
let re = sigma (1,2,f)
let _ =
print_int re
*)

