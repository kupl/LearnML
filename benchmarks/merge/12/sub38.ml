let rec merge (lst1, lst2) =
match lst1, lst2 with
|[], lst -> lst
|lst, [] -> lst
|h1::t1, h2::t2 -> if (h1 > h2 ) then h1::(merge (t1, lst2))
		else h2::(merge (lst1, t2))
