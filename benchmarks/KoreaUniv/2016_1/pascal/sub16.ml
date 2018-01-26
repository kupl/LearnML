let rec pascal (n1, n2) =
	match (n1, n2) with
	(n1, 0) -> 1
	|n1 = n2 -> 1
	|_ -> pascal (n1-1, n2-1) + pascal (n1-1, n2);;
