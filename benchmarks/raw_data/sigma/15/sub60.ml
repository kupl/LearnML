let increase = fun x -> x+1
let rec sigma (x,y,f) =
	if x>y then 0
	else f(x)+sigma(x+1,y,f)


