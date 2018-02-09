(*********************)
(*     Problem 1     *)
(*********************)
exception There_Is_No_Element

let rec fold1 f l =
	match l with
	| [] -> raise There_Is_No_Element
	| hd::[] -> hd
	| hd::tl -> f hd (fold1 f tl)

let rec max : int list -> int
= fun lst -> fold1 (fun x y -> if x > y then x else y) lst

let rec min : int list -> int
= fun lst -> fold1 (fun x y -> if x < y then x else y) lst
