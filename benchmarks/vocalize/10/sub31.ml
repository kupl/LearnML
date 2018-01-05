exception Error of string
let vocalize : string -> string list list =
	let read c =
		match c with '1' -> "일"
			|'2' -> "이"
			|'3' -> "삼"
			|'4' -> "사"
			|'5' -> "오"
			|'6' -> "육"
			|'7' -> "칠"
			|'8' -> "팔"
			|'9' -> "구" in
	let rec vocal str =
		if String.length str = 1 then (if (String.get str 0) = '0' then [] else [(read (String.get str 0))])
		else (if (String.get str 0) = '0' then (vocal (String.sub str 1 ((String.length str) -1)))
		      else (read (String.get str 0))::(match String.length str with 2 -> "십"
			      							|3 -> "백"
										|4 -> "천" )::(vocal (String.sub str 1 ((String.length str) -1)))) in
	fun str ->
	match String.length str with
		7 -> (vocal (String.sub str 0 3)) :: [(vocal (String.sub str 3 4))]
		|8 -> (vocal (String.sub str 0 4)) :: [(vocal (String.sub str 4 4))]
		|_ -> raise (Error "string length should be 7 or 8")
