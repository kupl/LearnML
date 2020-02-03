exception Problem

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
 