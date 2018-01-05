exception Error of string

let rec sigma a b f =
	if a == b then f b else (if a < b then f a + sigma (a + 1) b f else raise (Error "sigma : a가 b보다 큼"))
