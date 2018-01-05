let rec merge (a, b) = merge_2 a b []
and merge_2 lst_a lst_b lst_t =
	if lst_a = [] then lst_t @ lst_b else
	if lst_b = [] then lst_t @ lst_a else
	if(List.hd lst_a >= List.hd lst_b) 
	then merge_2 (List.tl lst_a) lst_b (lst_t @ [List.hd lst_a]) else
	if(List.hd lst_b >= List.hd lst_a)
       	then merge_2 lst_a (List.tl lst_b) (lst_t @ [List.hd lst_b]) else []
