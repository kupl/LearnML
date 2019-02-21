(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> match lst with
	|[] -> 0
	|h::[]->h
	|h::t->
		let x = max t in
		if h > x then h else x;;

let rec min : int list -> int
= fun lst -> match lst with
	|[] -> 0
	|h::[] -> h
	|h::t -> 
		let x = min t in
		if h < x then h else x;;
