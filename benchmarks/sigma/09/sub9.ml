(* 2006-11782 Song Young-chan, Hw1-1 Sigma *)

exception Error of string

let rec sigma a b f =
	if a>b then raise (Error "invalid arg")
	if a=b then (f a)
	else (f a)+(sigma (a+1) b f)
