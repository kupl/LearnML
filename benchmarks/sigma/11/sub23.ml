(*2009-11718 1-1*) 

let rec sigma(a,b,f) =
	if b<a then raise (Invalid_argument "error")
	else if a=b then (f a)
	else (f a) + sigma(a+1, b, f)
	
(* f ÇÔ¼ö´Â ¹»·Î *)
