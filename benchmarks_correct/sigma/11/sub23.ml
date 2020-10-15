(*2009-11718 1-1*) 

let rec sigma f a b =
	if b<a then raise (Invalid_argument "error")
	else if a=b then (f a)
	else (f a) + sigma f (a+1) b
	
(* f ÇÔ¼ö´Â ¹»·Î *)
