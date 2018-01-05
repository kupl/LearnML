(* CSE/ 2004-11920 / Yeseong Kim/ Prob 3*)

exception Error of string 

let vocalize str =
	let rec isAllZero str i = (*��� 0���� Ȯ���Ѵ�*)
		if ((String.length str) = i) then true
		else if (str.[i] != '0') then false
		else (isAllZero str (i+1))
	in
	let digit_to_vocal str n = (*���� �ϳ��� ���ڷ�*)
		match (str, n) with
			("0", 3) -> ["��"]
		|	("1", 3) -> ["��"]
                |	("2", _) -> ["��"]
                |	("3", _) -> ["��"]
                |	("4", _) -> ["��"]
                |	("5", _) -> ["��"]
                |	("6", _) -> ["��"]
                |	("7", _) -> ["ĥ"]
                |	("8", _) -> ["��"]
                |	("9", _) -> ["��"]
		|	_ -> []
	in
	let position_to_str n = (*�ڸ����� ���ڷ�*)
		match n with
			0 -> ["õ"]
		|	1 -> ["��"]
		|	2 -> ["��"]
		|	_ -> []
	in
	let makeString cStr n = (*���� �ڸ��� ���ڸ� ����� ��ȯ*)
		if (n = 3) then (digit_to_vocal cStr n)
		else if (cStr = "0") then []
		else (digit_to_vocal cStr n)@(position_to_str n)
	in
	let rec subVocalize str n = (*4���� �������� ����Ʈ�� ��´�*)
		if ((String.length str) = 0) then []
		else if (n = 3) then
			if ((str.[3] != '0') || (isAllZero str 0)) then (makeString (String.sub str n 1) n)
			else []
		else (makeString (String.sub str n 1) n)@(subVocalize str (n+1))
	in
	match (String.length str) with (*���ڸ� ������*)
		7 -> (subVocalize (String.concat "" ["0";(String.sub str 0 3)]) 0) :: [(subVocalize (String.sub str 3 4) 0)]
	|	8 -> (subVocalize (String.sub str 0 4) 0) :: [(subVocalize (String.sub str 4 4) 0)]
	|	_ -> raise (Error "Length is not 7 or 8")
