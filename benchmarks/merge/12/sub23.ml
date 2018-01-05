(*2009-11718 1-1*)
type merge = int list * int list -> int list

let rec merge (l1, l2) =
	match (l1, l2) with
	| ([], _) -> l2
	| (_, []) -> l1
	| (h1::t1, h2::t2) -> 
		if h1>h2 then (h1+0)::merge(t1, l2)
			else (h2+0)::merge(l1, t2)

