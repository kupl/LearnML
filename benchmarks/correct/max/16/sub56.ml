let rec fold f l a =
	match l with
	| [] -> a
	| h::t -> f h (fold f t a)

(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> fold (fun x y -> if x > y then x else y) lst min_int
 