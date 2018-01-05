let rec zipper l =
	match l with
		(h1::t1, h2::t2) -> h1::h2::[] @ zipper (t1,t2)
		|(l1, []) -> l1
		|([], l2) -> l2
		|([], []) -> []
		