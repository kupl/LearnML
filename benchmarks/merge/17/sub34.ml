let rec merge: (int list * int list) -> int list = fun (a,b) ->
	if List.hd a  > List.hd b then
		if (List.length a) > 1 then
			List.cons (List.hd a) (merge(List.tl a,b))	
		else
			List.append a b
	else
		if (List.length b) > 1  then
			List.cons (List.hd b) (merge(a,List.tl b))
		else
			List.append b a 
