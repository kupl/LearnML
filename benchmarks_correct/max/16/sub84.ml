(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst ->
match lst with
| [] -> 0
| hd::tl ->
	let rec fold f l a =
	match l with
	| [] -> a
	| hd::tl -> f hd (fold f tl a) in
		let compare x y =
		if x>y then x else y in
		fold compare tl hd;;
 