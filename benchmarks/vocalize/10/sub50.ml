exception UndefinedToken;;
exception IllegalLength;;

let read s d =
	(match s 
		with "1" -> if d=1 then ["일"] else []
		| "2" -> ["이"]
		| "3" -> ["삼"]
		| "4" -> ["사"]
		| "5" -> ["오"]
		| "6" -> ["육"]
		| "7" -> ["칠"]
		| "8" -> ["팔"]
		| "9" -> ["구"]
		| "0" -> []
		| _ -> raise UndefinedToken
	)@(match d
		with 1 -> []
		| 2 -> if s="0" then [] else ["십"]
		| 3 -> if s="0" then [] else ["백"]
		| 4 -> if s="0" then [] else ["천"]
		| _ -> raise UndefinedToken
	);; 

let rec vocalize s =
let n = String.length s in
let young sl =
	if sl = [] then ["영"] else sl
in
let rec iter s =
	let n = String.length s in
	if n>1 then
		(read (String.sub s 0 1) n)
		@ (iter (String.sub s 1 (n-1)))
	else (read (String.sub s 0 1) n)
in
	if n=7 then
		[(young (iter (String.sub s 0 3))); (young (iter (String.sub s 3 4)))]
	else if n=8 then
		[(young (iter (String.sub s 0 4))); (young (iter (String.sub s 4 4)))]
	else raise IllegalLength;;

let debug x = match x with
	[a;b] -> print_string ((String.concat "" a) ^ " " ^ (String.concat "" b) ^ "\n")

