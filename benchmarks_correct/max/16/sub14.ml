(*********************)
(*     Problem 1     *)
(*********************)
exception Invalid_input

let rec fold1 f l a =
	match l with
	| [] -> a
	| hd::tl -> f hd (fold1 f tl a)

let rec max : int list -> int
= fun lst ->  (* TODO *)
	match lst with
	| [] -> raise Invalid_input
	| hd::tl -> fold1 (fun x y -> if x>y then x else y) lst min_int
 