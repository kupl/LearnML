(* 2007-11651 KIM DONG HYUN *)

exception Error of string

let rec iter n f =
	if n < 0 then raise (Error "invalid arg")
	else if n = 0 then (fun x -> x)
	else (fun x -> f (iter (n-1) f x))