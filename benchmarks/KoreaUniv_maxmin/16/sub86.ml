exception Problem

let rec fold f l a =
	match l with
	|[] -> a
	|hd::tl -> f hd (fold f tl a)

let rec fold_base f l =
        match l with
        |[] -> raise Problem
	|hd::[] -> hd
        |hd::tl -> f hd (fold_base f tl)

(*********************)
(*     Problem 1     *)
(*********************)
let max : int list -> int = fun l -> 
	fold_base (fun x y -> if x > y then x else y) l

let min : int list -> int  = fun l ->
	fold_base (fun x y -> if x < y then x else y) l
