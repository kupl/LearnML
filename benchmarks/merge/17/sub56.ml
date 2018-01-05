let rec merge (x, y) =
	match x, y with
	| [], _ -> y
	| _, [] -> x
	| hdx :: tlx, hdy :: tly ->
		if hdx > hdy then
			hdx :: merge (tlx, y)
		else
			hdy :: merge (x, tly)
