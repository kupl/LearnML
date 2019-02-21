(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a =
	match l with
	| [] -> a
	| hd::tl -> f hd (fold f tl a)

let max : int list -> int 
= fun lst -> 
	(match lst with
	| [] -> raise (Failure "Empty list cannot have max value")
	| _ -> fold (fun x y -> if x >= y then x else y) lst min_int)

let min : int list -> int
= fun lst ->
	(match lst with
	| [] -> raise (Failure "Empty list cannot have min value")
	| _ -> fold (fun x y -> if x <= y then x else y) lst max_int)
