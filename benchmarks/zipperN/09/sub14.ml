let rec zipperN l =
	if l = [] then []
	else 
		let l' = List.filter (fun x -> x!=[]) l in 
		(List.map List.hd l') @ (zipperN (List.map List.tl l'));;
