let rec sigma ((a:int),(b:int),(f:int->int)): int =
	if a>b then 0 else f(a)+sigma(a+1,b,f)