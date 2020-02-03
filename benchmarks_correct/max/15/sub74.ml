let rec max : int list -> int
=fun l ->  
	match l with
	|[] -> 0
	|d::[] -> d
	|d::e -> 
		let f = max e in
		if d > f then d
		else f
 