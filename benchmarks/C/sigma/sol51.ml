exception Error of string
let rec sigma f a b =
	if a = b then f a
	else if a < b then
			sigma f (a+1) b + (f a)
		 else raise (Error "Abnormal Boundary")

;;
