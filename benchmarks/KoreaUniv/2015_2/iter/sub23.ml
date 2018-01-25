let rec iter (n,f) =
	fun a ->
	if n = 0 then a else
	iter (n-1,f) (f a)
