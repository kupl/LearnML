(*********************)
(*     Problem 1     *)
(*********************)
exception Problem

let rec fold f l =
match l with
|[]->raise Problem
|[i]->i
|hd::tl -> f hd (fold f tl);;

let rec max : int list -> int
 = fun lst -> fold (fun a b -> if a>b then a else b) lst ;;

let rec min : int list -> int
 = fun lst ->fold (fun a b -> if a>b then b else a) lst ;;