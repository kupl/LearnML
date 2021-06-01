
(* 2008-11720 Á¶°Ü¸® *)

let rec sigma f a b =
if (a=b) then (f b)
	else (f a)+(sigma f (a+1) b)

