let rec merge(lst1: int list) (lst2: int list)  =
  match lst1 with
  | [] -> lst2
  | h1::t1 -> match lst2 with
	    | [] -> lst1
	    | h2::t2 -> if h1 >= h2 then h1 :: (merge t1 lst2)
			else h2 :: (merge lst1 t2)
