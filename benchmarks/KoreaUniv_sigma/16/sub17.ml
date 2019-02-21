exception Illegal_input
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b ->  (* TODO *)
	if (a)>(b) then raise Illegal_input
	else sum f a b

