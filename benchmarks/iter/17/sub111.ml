let rec iter : (int * ('a -> 'a)) -> ('a -> 'a) = fun (n, fn) ->
	let nest ((fn1, fn2) : (('a -> 'a) * ('a -> 'a))) (x : 'a) : 'a = fn1 (fn2 x) in
	if n <= 0 then (fun x -> x)
	else nest (fn, iter(n-1, fn))
