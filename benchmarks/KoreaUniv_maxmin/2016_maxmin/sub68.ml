let rec fold f l a =
	match l with
	[] -> a
	| hd::tl -> f hd (fold f tl a);;


let rec fold_for_Pro1 f l =
	match l with
	[] -> raise (Failure "The array is empty")
	| hd::[] -> hd
	| hd::tl -> f hd (fold_for_Pro1 f tl);;

(*********************)
(*     Problem 1     *)
(*********************)

let rec max : int list -> int
= fun lst -> fold_for_Pro1 (fun x y -> if (x>y) then x else y) lst;;

let rec min : int list -> int
= fun lst -> fold_for_Pro1 (fun x y -> if (x>y) then y else x) lst;;
