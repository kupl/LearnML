(* C:\Users\owner\Desktop\Homework 1(3).ml *)

(* Syntax Error or Unbound Value
exception Error string;;
 *)

exception Error of string;;]

let rec vocalize strin =
	let vocal1 strin =
		match strin with
		| '1' -> []
		| '2' -> ["ÀÌ"]
		| '3' -> ["»ï"]
		| '4' -> ["»ç"]
		| '5' -> ["¿À"]
		| '6' -> ["À°"]
		| '7' -> ["Ä¥"]
		| '8' -> ["ÆÈ"]
		| '9' -> ["±¸"]
		| _ -> raise (Error "FAIL1!") in

	let vocalLength strin =
		match (String.length strin) with
		4 -> ["Ãµ"]
		| 3 -> ["¹é"]
		| 2 -> ["½Ê"]
		| _ -> raise (Error "FAIL2!") in

	let rec vocal4 strin =
		match strin with
		"0000" -> ["¿µ"]
		| "1" -> ["ÀÏ"]
		| _ -> 	match (String.length strin) with
			1 -> 	if (compare (String.get strin 0) '0') == 0 then []
				else vocal1(String.get strin 0)
			| _ ->	match (String.get strin 0) with
				'0' -> vocal4 (String.sub strin 1 ((String.length strin) - 1))
				| _ -> (vocal1 (String.get strin 0)) @ (vocalLength strin) @ vocal4 (String.sub strin 1 ((String.length strin) - 1))
		in


	if ((String.length strin) != 8) && ((String.length strin) != 7) then raise (Error "FAIL3!")
	else if (String.length strin) != 8 then vocalize ("0" ^ strin)
	else [(vocal4 (String.sub strin 0 4)) ; (vocal4 (String.sub strin 4 4))] ;;

