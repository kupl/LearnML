exception Error_of_string

let rec sigma (a, b, f) =
	if a>b then raise Error_of_string
	else
	if a=b then
		f b
		else (f a) + sigma ((a+1), b, f);;

(*
let f = function a -> a*2;;
let g a = a*3;;
let (h : int -> int) = function a -> a*4;;
*)
