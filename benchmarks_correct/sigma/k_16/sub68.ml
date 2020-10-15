let rec sigma (f : int -> int)  (x:int) (y:int) : int =
	if y - x = 0 then (f x)
	else (f x) + (sigma f (x+1) (y))
