let rec mergesub ((lst1:int list), (lst2:int list), (result: int list)): int list = 
	if (lst1 == []) then (List.append result lst2)
	else if  (lst2 == []) then (List.append result lst1)
	else 
		if (List.hd lst1 > List.hd lst2) then mergesub(List.tl lst1, lst2, (List.append result [List.hd lst1]))
		else mergesub(lst1, List.tl lst2, (List.append result [List.hd lst2]))

let merge ((lst1:int list), (lst2:int list)): int list = 
	mergesub(lst1, lst2, [])

