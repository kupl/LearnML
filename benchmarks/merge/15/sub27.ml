let rec merge ( a  , b  ) = 
	if a = [] then b
	else if b = [] then a
	else (
		if (List.hd a) >= (List.hd b) then List.hd a :: merge ((List.tl a), b)
		else List.hd b :: merge (a, (List.tl b))
		
	)