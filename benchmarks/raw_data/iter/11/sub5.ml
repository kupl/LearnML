(*2006-11681 °­Çö¼®*)
exception Invaild_input of string

let rec iter (n,f) x =
	match n with
	0 -> x
	| 1 -> (f x)
	| _ -> if (n>1) then (f (iter (n-1,f) x)) else raise (Invaild_input "n<0")
