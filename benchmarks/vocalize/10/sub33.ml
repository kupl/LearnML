exception Error of string

let vocalize pn =
	let zero lst = match lst with [] -> ["영"] | _ -> lst in
	let rec vocal_c s =
		let vocal_u i = match i with
		  "0" -> [] | "1" -> [] | "2" -> ["이"] | "3" -> ["삼"] 
		| "4" -> ["사"] | "5" -> ["오"] | "6" -> ["육"] 
		| "7" -> ["칠"] | "8" -> ["팔"] | "9" -> ["구"] | _ -> raise (Error "vocalize : input is not number")
		in
		let h = String.sub s 0 1 in
		match String.length s with
		  1 -> if s = "1" then ["일"] else vocal_u s
		| 2 -> (vocal_u h) @ (if h="0" then [] else ["십"]) @ (vocal_c (String.sub s 1 1))
		| 3 -> (vocal_u h) @ (if h="0" then [] else ["백"]) @ (vocal_c (String.sub s 1 2))
		| 4 -> (vocal_u h) @ (if h="0" then [] else ["천"]) @ (vocal_c (String.sub s 1 3))
		| _ -> raise (Error "this can't be happen")
	in
	match String.length pn with
	  8 -> [zero (vocal_c (String.sub pn 0 4)); zero (vocal_c (String.sub pn 4 4))]
	| 7 -> [zero (vocal_c (String.sub pn 0 3)); zero (vocal_c (String.sub pn 3 4))]
	| _ -> raise (Error "vocalize : invalid input string length")

	(*
let rec fold list f =
	match list with
	  [] -> []
	| h::t -> (f h)::(fold t f);;

let un s =
  match s with "이" -> 2 | "삼" -> 3 | "사" -> 4 | "오" -> 5 | "육" -> 6 | "일" -> 1 | "이" -> 2 | "삼" -> 3 | "사" -> 4 | "오" -> 5 | "육" -> 6 | "칠" -> 7 | "팔" -> 8 | "구" -> 9 | "십" -> 10 | "백" -> 100 | "천" -> 1000
  *)
