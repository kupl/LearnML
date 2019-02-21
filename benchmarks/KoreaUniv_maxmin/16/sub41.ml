(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a = 
match l with
	| [] -> a
  	| [x] -> x
  	| hd::tl -> f hd (fold f tl a)

let compareMax : int -> int -> int
= fun a b -> if a > b then a else b

let rec max : int list -> int
= fun lst -> fold compareMax lst 0

let compareMin : int -> int -> int
= fun a b -> if a < b then a else b

let rec min : int list -> int
= fun lst -> fold compareMin lst 0
