(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> 0 (* TODO *)

let rec min : int list -> int
= fun lst -> 0 (* TODO *)

let who_is_G x y = if x>y then x else y;;
let who_is_S x y = if x>y then y else x;;

let rec fold f l a =
	match l with
	| []->1
	| hd::tl -> f hd (fold f tl a);;

let max lst = fold(who_is_G) lst 1;;
let min lst = fold(who_is_S) lst 1;;
