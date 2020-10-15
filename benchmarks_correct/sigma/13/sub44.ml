let rec sigma k i j =
	match (i, j, k) with
	| (p, q, f) ->
		if p = q then (f p)
		else if p > q then 0
		else (f p) + sigma f (p+1) q
