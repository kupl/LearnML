let rec iter ((n:int),f)=
	match n with
	0-> fun (x:int) -> x 
	|_-> fun (x:int) -> f (iter(n-1,f) x)
