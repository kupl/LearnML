let rec filter pred lst =
	match lst with
	| [] -> []
	| hd::tl -> if pred hd = true then (filter pred tl) @ [hd]
							else filter pred tl

