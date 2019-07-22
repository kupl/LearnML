let rec filter pred lst = 
	match lst with
	| [] -> []
	| h::t -> if (pred h) then (h::filter pred t)
		else (filter pred t);;
