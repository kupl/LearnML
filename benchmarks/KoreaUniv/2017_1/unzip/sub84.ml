(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> 
	match lst with
	|[]->([],[])
	|hd::tl-> match hd with 
			|(a,b) -> let l1, l2 = unzip tl in a::l1, b::l2;;