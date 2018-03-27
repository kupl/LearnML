(*********************)
(*  functions needed *)
(*********************)
let rec fold f l a =
	match l with
	| [] ->	a
	| hd::tl ->	f hd (fold f tl a)

let rec nth l n =
	match l with
	| [] ->	raise (Failure "List is shorter than expected.")
	| hd::tl ->	if n <= 0 then hd else nth tl (n - 1)

(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> 
	fold (fun x y -> if x > y then x else y) lst (nth lst 0)

let rec min : int list -> int
= fun lst ->
	fold (fun x y -> if x < y then x else y) lst (nth lst 0)
