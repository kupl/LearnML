(*********************)
(*     Problem 1     *)
(*********************)
let comp1 x y = if x >=  y then x else y;;

let rec fold f l a =
		match l with
		| [] -> a
		| hd::tl -> f hd (fold f tl a)
let rec max : int list -> int
= fun lst -> fold comp1 lst (-100000000)
 