let rec sigma (i, j, k) =
	match (i, j, k) with
	| (p, q, f) ->
		if p = q then (f p)
		else if p > q then 0
		else (f p) + sigma((p + 1), q, f)
