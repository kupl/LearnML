let rec merge : int list * int list -> int list =
	fun (lst1, lst2) ->
		match lst1, lst2 with
		| [], [] -> []
		| [], lst2 -> lst2
		| lst1, [] -> lst1
		| hd1::tail1, hd2::tail2 ->
			if hd1 > hd2 then hd1::(merge(tail1, lst2))
			else  hd2::(merge(lst1, tail2))