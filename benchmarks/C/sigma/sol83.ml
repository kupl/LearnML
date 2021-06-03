exception InvalidInput		(* Exception : a>bÀÏ °æ¿ì *)

let rec sigma f a b =
	if a>b then raise InvalidInput
	else if a=b then (f a)
	else (f b) + (sigma f a (b-1))
