let rec fac n =
	if n>1 then n*fac(n-1)
	else 1;;
let rec pascal : int * int -> int
= fun (n1, n2) -> if n1>=n2 then fac(n1)/fac(n2)/fac(n1-n2)
							    else 1;;
