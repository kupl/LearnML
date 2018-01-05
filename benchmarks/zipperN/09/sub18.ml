let rec zipperN l =
	match l with
		h1::t1 -> (match h1 with 
				h::t -> h::zipperN (t1 @ [t])
				|[] -> zipperN t1)
		|[] -> []
				