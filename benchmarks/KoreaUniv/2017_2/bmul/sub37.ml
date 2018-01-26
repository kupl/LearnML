(* ------------------problem7------------------ *)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> (* TODO *)
	let rec twoPower n = 
		match n with
		| 0 -> 1
		| 1 -> 2
		| _ -> 2 * twoPower (n-1) in
	let rec binToInt b = 
		match b with
		| [] -> 0
		| hd::tl -> if hd=ONE then (twoPower ((List.length b)-1)) + (binToInt tl)
					else binToInt tl in
	let rec intToBin n = 
		match n with
		| 0 -> []
		| _ -> if (n mod 2) = 0 then intToBin (n/2) @ [ZERO]
				else intToBin (n/2) @ [ONE] in

	let tmp1 = binToInt b1 in
	let tmp2 = binToInt b2 in
	intToBin (tmp1*tmp2);;


(* #use "hw2.ml";; *)










