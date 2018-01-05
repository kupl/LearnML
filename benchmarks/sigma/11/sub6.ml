


let rec sum(a, b, f) =
	if a>b then raise(Invalid_argument "sigma")
	else if a=b then f a
	else (f a) + sum(a+1, b, f)

(*let _= print_int (sum(1,3,fun n -> n))*)

 
