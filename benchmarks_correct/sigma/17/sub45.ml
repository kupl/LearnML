let rec sigma : int * int * (int -> int) -> int = fun (min, max, func) ->
	if min > max then 0
	else if min = max then func min
	else func min + sigma (min + 1, max, func)
