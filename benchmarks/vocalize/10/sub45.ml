(* complete *)
exception Invalid_Input

let vocalize str =
	let front = if String.length str = 7
			then String.sub str 0 3
			else if String.length str = 8
				then String.sub str 0 4
				else raise Invalid_Input
		in
	let back = if String.length str = 7
			then String.sub str 3 4
			else if String.length str = 8
				then String.sub str 4 4
				else raise Invalid_Input
		in
	let one n = 
		match n with
		"1" -> "일"
		|"2" -> "이"
		|"3" -> "삼"
		|"4" -> "사"
		|"5" -> "오"
		|"6" -> "육"
		|"7" -> "칠"
		|"8" -> "팔"
		|"9" -> "구"
		|_ -> raise Invalid_Input
		in
	let ten n =
		match n with
		2 -> "십"
		|3 -> "백"
		|4 -> "천"
		|_ -> raise Invalid_Input
		in
	let rec getlst st =
		if String.length st = 1
			then (if st = "0" 
				then []
				else [one (String.sub st 0 1)])
			else match (String.sub st 0 1) with
				"0" -> getlst (String.sub st 1 ((String.length st) - 1))
				|"1" -> (ten (String.length st))::(getlst (String.sub st 1 ((String.length st) - 1)))
				| _ -> (one (String.sub st 0 1))::(ten (String.length st))::(getlst (String.sub st 1 ((String.length st) - 1)))
		in
	if (((String.length str) = 7)||((String.length str) = 8))
		then (
			match (getlst front,getlst back) with
			([],[]) -> [["영"];["영"]]
			|([],b) -> [["영"];b]
			|(a,[]) -> [a;["영"]]
			|(a,b) -> [a;b]
		)
		else raise Invalid_Input
;;

