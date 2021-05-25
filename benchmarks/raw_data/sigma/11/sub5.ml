(*2006-11681 °­Çö¼®*)
exception Invalid_argument of string

let rec sigma (a,b,f)=
	if a>b then raise (Invalid_argument "sigma:a>b") 
	else if a=b then (f b)
	else (f a) + sigma ((a+1),b,f)
