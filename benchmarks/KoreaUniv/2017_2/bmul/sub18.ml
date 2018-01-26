(*problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec pow2 : int -> int
= fun n -> if n = 0 then 1 else if n = 1 then 2 else 2 * (pow2 (n-1))

let rec b2t : bin -> int
= fun b ->
match (List.length b) with
0 -> raise (Failure "list is too short")
|1 -> 
	(match b with 
		|[ZERO] -> 0
		|[ONE] -> 1
		|_ -> raise (Failure "Type Error"))
|_ -> (pow2 ((List.length b) - 1)) * (b2t [List.hd b]) + b2t (List.tl b)

let rec t2b : int -> bin
= fun n -> if n = 0 then [ZERO] else if n = 1 then [ONE] else List.append (t2b (n/2)) (t2b (n mod 2))

let bmul : bin -> bin -> bin
= fun b1 b2 -> t2b ((b2t b1) * (b2t b2))
