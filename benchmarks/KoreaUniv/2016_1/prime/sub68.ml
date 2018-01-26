let rec pr_1 x y =
		if y = 1 then 1
		else if x mod y =  0 then 0
		else pr_1 x (y-1)

let prime a =
	if pr_1 a (a-1) = 1 then true
	else false
