let rec iter : (int * ('a -> 'a)) -> ('a -> 'a) = fun (n, fn) ->
	let nest : ('a -> 'a) * ('a -> 'a) -> 'a -> 'a
  =fun (fn1,fn2) x -> fn1 (fn2 x) in
	if n <= 0 then (fun x -> x)
	else nest (fn, iter(n-1, fn))
