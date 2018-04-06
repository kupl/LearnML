let rec fold f l a =
	match l with
	| [] -> a
	| hd::tl -> if tl=[] then hd else f hd (fold f tl a)

let rec max : int list -> int
= fun lst -> 
	fold (fun x y -> if x > y then x else y) lst 0

let rec min : int list -> int
= fun lst ->
	fold (fun x y -> if x<y then x else y) lst 0
