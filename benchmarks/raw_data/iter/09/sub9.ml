(* 2006-11782 Song Young-chan, Hw1-2 iter *)

exception Error of string

let rec iter(n,f) =
	if n<0 then raise (Error "invalid arg") 
	else if n=0 then (fun x -> x)
	     else (fun x -> (iter(n-1,f) (f x)))
