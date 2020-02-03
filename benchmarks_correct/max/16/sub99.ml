let rec fold f l a =
	match l with
	|[] -> a
	|hd::tl -> f hd (fold f tl a)

(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> 
	match lst with
	|[] -> raise (Failure "Input is not correct")
	|hd::tl -> fold (fun x y -> if x>y then x else y) lst hd
