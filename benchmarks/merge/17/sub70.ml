let rec merge : int list * int list -> int list = fun (a, b) ->
match (a, b) with 
| a, [] -> a
| [], b -> b
| h1::t1, h2::t2 ->
	if(h1 > h2) then
		h1 :: merge(t1, h2::t2)
	else
		h2 :: merge(h1::t1, t2)


