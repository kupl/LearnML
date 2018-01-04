let rec zipper l =
	match l with
		([], []) -> []
		|(l1, []) -> l1
		|([], l2) -> l2
		|(h1::t1, h2::t2) -> h1::h2::[] @ zipper (t1,t2)

		