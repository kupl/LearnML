

(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a =
	match l with
	|[] -> a
	|[n] -> n
	|hd::tl -> f hd (fold f tl a)

let max l = fold (fun x y -> if x>y then x else y) l 0
let min l = fold (fun x y -> if x<y then x else y) l 0

