exception Error_of_string

let rec sigma f a b =
	if a>b then raise Error_of_string
	else
	if a=b then
		f b
		else (f a) + sigma f (a+1) b;;

(*
let f = function a -> a*2;;
let g a = a*3;;
let (h : int -> int) = function a -> a*4;;
*)
