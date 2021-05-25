
(* 2008-11720 Á¶°Ü¸® *)

let rec sigma (a, b, f) =
if (a=b) then (f b)
	else (f a)+(sigma (a+1, b, f))

