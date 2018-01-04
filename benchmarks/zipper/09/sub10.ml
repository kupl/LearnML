let rec zipper ((a:int list),(b:int list)) =
	if List.length a = 0 then b
	else if List.length b = 0 then a
	else List.hd a::List.hd b::zipper(List.tl a, List.tl b)
