(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
	if n < 0 then raise (Failure "cannot iterate minus times") else
	match n with
	|0 -> (fun x -> x)
	|_ -> (fun x -> f((iter (n-1, f)) x))