(*real code start*)
let rec merge((list1 : int list), (list2 : int list)): int list =
match (list1, list2) with
| ([], []) -> []
| (hd::tl, []) -> hd :: merge(tl, []) 
| ([], hd::tl) -> hd :: merge(tl, []) 
| (hd::tl, hd2::tl2) -> if (List.hd list1 > List.hd list2)
		      then (List.hd list1)::merge(List.tl list1 , list2)
			else (List.hd list2)::merge(list1, List.tl list2)

(*real code end*)
