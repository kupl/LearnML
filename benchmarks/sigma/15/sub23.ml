let rec sigma s e f =
	match s with
	s -> if (s <= e) then f s else (f s) + sigma (s+1) e f
