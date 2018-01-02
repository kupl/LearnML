let rec f pred lst =
	match lst with
	| [] -> []
	| hd::tl -> if pred hd = true then (f pred tl) @ [hd]
							else f pred tl