(*
let rec iter : int * (int -> int) -> (int -> int)
= fun (n, f) ->
	if n < 0 then raise (Failure "Invalid")
	else if n = 0 then (fun x -> x)
	else (fun x -> f ((iter (n-1, f)) x))
*)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n, f) x ->
	if n < 0 then raise (Failure "Invalid")
	else if n = 0 then x
	else (f ((iter (n-1, f)) x))