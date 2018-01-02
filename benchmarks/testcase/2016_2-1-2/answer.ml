let max_int = 4611686018427387903;;

let rec fold f l a =
	match l with
	| [] -> a
	| hd::tl -> f hd (fold f tl a);;

let rec min : int list -> int
= fun lst ->
	fold (fun x y -> if x < y then x else y) lst max_int;;