(*********************)
(*     Problem 1     *)
(*********************)
 
let who_is_G x y = if x>y then x else y;;
 
let rec fold f l a =
	match l with
	| []->1
	| hd::tl -> f hd (fold f tl a);;

let max lst = fold(who_is_G) lst 1;;
 