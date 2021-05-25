(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l=
	match l with
		| [] -> 0
		| [a] -> a
		| hd::tl -> if f hd (fold f tl) then hd else (fold f tl)

let rec filter pred lst=
	match lst with
		| [] -> []
		| hd::tl -> if pred hd then hd::(filter pred tl) else filter pred tl