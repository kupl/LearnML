(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a =
	match l with
	 | [] -> a
	 | hd::tl -> f hd (fold f tl a)

let rec max : int list -> int
= fun lst -> match lst with
			| [] -> raise(Failure "empty list")
			| hd::tl -> fold (fun x y -> if x>y then x else y) tl hd

let rec min : int list -> int
= fun lst -> match lst with
			| [] -> raise(Failure "empty list!")
			| hd::tl -> fold (fun x y -> if x<y then x else y) tl hd 
