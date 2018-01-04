let rec zipperN((n:int list list)) =
	let rec gethd (a:int list list) =
		if a=[] then []
		else if a=[[]] then []
		else if List.length (List.hd a) = 0 then gethd(List.tl a)
		else List.hd (List.hd a)::gethd(List.tl a) in
	let rec gettl (b:int list list) =
		if b=[] then []
		else if b=[[]] then []
		else if List.length (List.hd b) = 0 then gettl(List.tl b)
		else List.tl (List.hd b)::gettl(List.tl b) in
	if n = [] then []
	else if List.length(List.hd n) = 0 then zipperN(List.tl n)
	else gethd n@(zipperN(gettl n))
