(* 4190.310 Programming Language		*
 * Homework #1 - Exercise 3 (말해줘)	*
 * 2008-11744 Jongwook Choi 			*)

open String ;;
exception Error of string

let vocalize : string -> string list list = fun x ->
	if String.length(x) < 7 or String.length(x) > 8 then
		raise (Error "The length of string must be between 7 and 8, inclusive.")
	else
		let vocalizeDigit n = match n with
			  0 -> ["영"]
			| 1 -> []
			| 2 -> ["이"]
			| 3 -> ["삼"]
			| 4 -> ["사"]
			| 5 -> ["오"]
			| 6 -> ["육"]
			| 7 -> ["칠"]
			| 8 -> ["팔"]
			| 9 -> ["구"]
			| 10 -> ["십"]
			| 100 -> ["백"]
			| 1000 -> ["천"]
			| _ -> ["에러"]
		in
		(* 정수 n을 읽어서 string list로 ... *)
		let rec vocalizeInteger : int * bool -> string list = fun (n, isFirst) ->
			if n >= 10000 then
				["에러"]
			else if n >= 1000 then
				let q = n / 1000 in
				vocalizeDigit(q) @ vocalizeDigit(1000) @ vocalizeInteger(n mod 1000, false)
			else if n >= 100 then
				let q = n / 100 in
				vocalizeDigit(q) @ vocalizeDigit(100) @ vocalizeInteger(n mod 100, false)
			else if n >= 10 then
				let q = n / 10 in
				vocalizeDigit(q) @ vocalizeDigit(10) @ vocalizeInteger(n mod 10, false)
			else if n >= 2 || (n == 0 && isFirst) then
				vocalizeDigit(n)
			else if n == 1 then
				["일"]
			else
				[]
		in
		try
			let l = (String.length(x)) - 4 in
			let first = int_of_string(String.sub x 0 l) in
			let second = int_of_string(String.sub x l 4) in
			[ vocalizeInteger(first, true) ; vocalizeInteger(second, true) ]
		with Failure _ -> raise (Error "The input string cannot contain nondigit characters.") 


let see x = match x with
	[a; b] ->
			let _ = List.iter (fun x -> print_string x) a in
			let _ = print_string " " in
			let _ = List.iter (fun x -> print_string x) b in
			print_string "\n" 
	| _ -> ()


