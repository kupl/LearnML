(* problem 7 *)
type digit = ZERO | ONE
type bin = digit list


let rec helpB2d : bin -> bin (*reverse*)
= fun b1 -> match b1 with
	| [] -> []
	| hd::t1 -> (helpB2d t1) @ [hd]

let rec b2d : bin -> int
= fun b1 -> match b1 with
	| [] -> 0
	| hd::t1 -> match hd with
			| ONE -> 1 + 2 * (b2d t1)
			| ZERO -> 2 * (b2d t1)
let rec d2b : int -> bin
= fun n -> match n with
	| 0 -> [ZERO]
	| 1 -> [ONE]
	| _ -> if (n mod 2 = 0) then (d2b (n/2))@[ZERO] else (d2b ((n-1)/2))@[ONE] 

let bmul : bin -> bin -> bin
= fun b1 b2 -> d2b ( (b2d (helpB2d b1)) * (b2d (helpB2d b2)) )
