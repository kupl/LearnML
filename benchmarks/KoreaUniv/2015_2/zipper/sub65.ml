let rec zipper : int list * int list -> int list
=fun (a,b) ->
	if (a=[] && b=[]) then []
	else if (a=[]) then
		match b with
		| [] -> []
		| hd :: tl -> (hd)::(zipper (a, tl))
	else if (b=[]) then
		match a with
		| [] -> []
		| hd :: tl -> (hd)::(zipper (tl, b))
	else
		match a with
		| [] -> []
		| hd :: tl ->
			match b with
			| [] -> []
			| x :: y -> (hd)::(x)::(zipper (tl, y))
