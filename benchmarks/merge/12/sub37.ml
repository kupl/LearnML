let rec merge(a, b) = 
	match (a, b) with 
	| ([], _) -> b
	| (_, []) -> a
	| (aa::al, bb::bl) -> 
	  if aa > bb then aa::merge(al, b)
	  else bb::merge(a, bl)
