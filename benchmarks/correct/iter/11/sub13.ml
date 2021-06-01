exception OutofBound
let rec iter (n, f) x =
	if n>1 then iter(n-1, f) (f x)
	else if n=1 then (f x)
	else if n=0 then x
	else raise OutofBound

(*let _= print_int(iter(10, fun x -> 2+x) 0)*)
