(* CSE/ 2004-11920 / Yeseong Kim/ Prob 3*)

exception Error of string 

let vocalize str =
	let rec isAllZero str i = (*모두 0인지 확인한다*)
		if ((String.length str) = i) then true
		else if (str.[i] != '0') then false
		else (isAllZero str (i+1))
	in
	let digit_to_vocal str n = (*숫자 하나를 글자로*)
		match (str, n) with
			("0", 3) -> ["영"]
		|	("1", 3) -> ["일"]
                |	("2", _) -> ["이"]
                |	("3", _) -> ["삼"]
                |	("4", _) -> ["사"]
                |	("5", _) -> ["오"]
                |	("6", _) -> ["육"]
                |	("7", _) -> ["칠"]
                |	("8", _) -> ["팔"]
                |	("9", _) -> ["구"]
		|	_ -> []
	in
	let position_to_str n = (*자릿수를 글자로*)
		match n with
			0 -> ["천"]
		|	1 -> ["백"]
		|	2 -> ["십"]
		|	_ -> []
	in
	let makeString cStr n = (*현재 자리의 글자를 만들어 반환*)
		if (n = 3) then (digit_to_vocal cStr n)
		else if (cStr = "0") then []
		else (digit_to_vocal cStr n)@(position_to_str n)
	in
	let rec subVocalize str n = (*4글자 기준으로 리스트를 잡는다*)
		if ((String.length str) = 0) then []
		else if (n = 3) then
			if ((str.[3] != '0') || (isAllZero str 0)) then (makeString (String.sub str n 1) n)
			else []
		else (makeString (String.sub str n 1) n)@(subVocalize str (n+1))
	in
	match (String.length str) with (*글자를 나눈다*)
		7 -> (subVocalize (String.concat "" ["0";(String.sub str 0 3)]) 0) :: [(subVocalize (String.sub str 3 4) 0)]
	|	8 -> (subVocalize (String.sub str 0 4) 0) :: [(subVocalize (String.sub str 4 4) 0)]
	|	_ -> raise (Error "Length is not 7 or 8")
