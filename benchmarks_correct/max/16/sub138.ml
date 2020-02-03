(*********************)
(*     Problem 1     *)
(*********************)
(* choose_one's input list should be non-empty *)
let rec choose_one f lst =
	let rec loop e l =
		match l with
		|	[] -> e
		|	h :: t -> loop (f e h) t in
	match lst with [] -> 0 | h :: t -> loop h t

let rec max : int list -> int
= fun lst -> choose_one (fun a b -> if a > b then a else b) lst
 