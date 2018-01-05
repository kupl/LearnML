let rec merge (list1, list2) =
	match list2 with
	| [] -> list1
	| h :: t ->
		( match list1 with
			| [] -> list2
			| a :: b ->
			 if (a>h) then 
				 a::merge(b, list2) 
				 else
					 h :: merge(list1, t))
