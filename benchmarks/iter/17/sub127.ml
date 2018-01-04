
let rec iter (n, f) x =
	if (n > 0) then iter (n-1, f) (f x)
	else x;;

