exception Error of string

let int_str i = 
	match i with
	"1" -> "일"
	|"2" -> "이"
	|"3" -> "삼"
	|"4" -> "사"
	|"5" -> "오"
	|"6" -> "육"
	|"7" -> "칠"
	|"8" -> "팔"
	|"9" -> "구"
	|_ -> raise (Error "Error")

let get_digit i = 
	match i with
	2 -> "십"
	|3 -> "백"
	|4 -> "천"
	|_ -> raise (Error "Error")
	


let rec converter2 str = 
	let
		len = String.length str	in
	if len = 0
		then []
	else if String.sub str 0 1 = "0"
		then converter2 (String.sub str 1 (len-1))
	else if len = 1
		then int_str (String.sub str 0 1)::(converter2 (String.sub str 1 (len-1)))
	else if String.sub str 0 1 = "1"
		then (get_digit len)::(converter2 (String.sub str 1 (len-1)))
	else int_str (String.sub str 0 1)::get_digit len::(converter2 (String.sub str 1 (len-1)))

let converter str = 
	if str = "000" || str = "0000" 
		then ["영"]
	else converter2 str




let vocalize str = 
	if String.length str = 7 
		then (converter (String.sub str 0 3))::(converter (String.sub str 3 4))::[]
	else if String.length str = 8
		then (converter (String.sub str 0 4))::(converter (String.sub str 4 4))::[]
	else
		raise (Error "Input Error")
	
(*
let a = [(int_str "1"), (int_str "2"), (int_str "3"), (int_str "4"), (int_str "5"), (int_str "6"), (int_str "7"), (int_str "8"), (int_str "9")]
let b = [(get_digit 2), (get_digit 3), (get_digit 4)]
*)
