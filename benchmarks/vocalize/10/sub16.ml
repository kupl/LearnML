exception Notnum
exception Error_of_string

let readnum str =
	match str with
		"1" -> "일" 
		| "2" -> "이" 
		| "3" -> "삼"
		| "4" -> "사"
		| "5" -> "오"
		| "6" -> "육"
		| "7" -> "칠"
		| "8" -> "팔"
		| "9" -> "구"
		| _ -> raise Notnum;;

let rec readstr (str, length) =
	if length = 4 then
		if str = "0000" then ["영"]
		else 
			if str.[0] = '0' then readstr ((String.sub str 1 3), 3)
			else 
			if str.[0] = '1' then "천"::(readstr ((String.sub str 1 3), 3))
			else (readnum (String.sub str 0 1))::"천"::(readstr ((String.sub str 1 3), 3))
	else
	if length = 3 then
		if str.[0] = '0' then readstr ((String.sub str 1 2), 2)
		else 
		if str.[0] = '1' then "백"::(readstr ((String.sub str 1 2), 2))
		else (readnum (String.sub str 0 1))::"백"::(readstr ((String.sub str 1 2), 2))
	else
	if length = 2 then
		if str.[0] = '0' then readstr ((String.sub str 1 1), 1)
		else 
		if str.[0] = '1' then "십"::(readstr ((String.sub str 1 1), 1))
		else (readnum (String.sub str 0 1))::"십"::(readstr ((String.sub str 1 1), 1))
	else
		if str = "0" then []
		else (readnum str)::[];;

let vocalize str =
	if ((String.length str)<7)||((String.length str)>8) then raise Error_of_string
	else
	if (String.length str) = 7 then
		if (String.sub str 0 3) = "000" then [["영"]; readstr ((String.sub str 3 4), 4)]
		else [readstr ((String.sub str 0 3), 3); readstr ((String.sub str 3 4), 4)]
	else [readstr ((String.sub str 0 4), 4); readstr ((String.sub str 4 4), 4)];;
		
let rec  print slist =
	match slist with [slist1; slist2] ->
	print_string ("["^(String.concat ";" ["["^(String.concat ";" slist1)^"]" ; "["^(String.concat ";" slist2)^"]" ] )^"]")
	| _ -> raise Notnum
	;;
