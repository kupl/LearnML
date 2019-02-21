(*********************)
(*     Problem 1     *)
(*********************)

let rec fold f l a =
	match l with
	| [] -> a
	| hd::tl -> f hd (fold f tl a)

let rec max : int list -> int
= fun lst -> (* TODO *)
	fold (fun a b -> if(a>b) then a else b) lst min_int

let rec min : int list -> int
= fun lst -> (* TODO *)
	fold (fun a b -> if(b>a) then a else b) lst max_int
