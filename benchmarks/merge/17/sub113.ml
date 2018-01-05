let rec merge (x, y) = match x with 
	| [] -> y
	| hdx :: tlx -> match y with
		| [] -> x
		| hdy :: tly -> if (hdx >= hdy) then hdx :: merge(tlx, y)
						else hdy :: merge(x, tly)