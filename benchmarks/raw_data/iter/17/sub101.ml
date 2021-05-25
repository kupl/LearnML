let rec iter ((n : int), (f : 'a->'a)) (x : 'a) : 'a = 
	if n==0 then x
	else let y = f(x) in
	iter (n-1, f) y
;;
