(* 2007-11651 KIM DONG HYUN *)

exception Error of string

let rec sigma a b f =
	if a > b then raise (Error "invalid arg")
	else if a = b then (f a)
	else (f a) + (sigma (a+1) b f)