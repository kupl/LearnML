exception Error of string
let vocalize strr =
	let matching str =
		match str with
		"1"->"일"
		|"2"->"이"
		|"3"->"삼"
		|"4"->"사"
		|"5"->"오"
		|"6"->"육"
		|"7"->"칠"
		|"8"->"팔"
		|"9"->"구"
		|_ -> ""
	in
	let firstStr str = 
		if (String.length str = 7) then (String.sub str 0 3 )
		else (String.sub str 0 4 )
	in
	let lastStr str = 
		if (String.length str = 7) then (String.sub str 3 4)
		else (String.sub str 4 4)
	in
	let rec listUnion a b = 
		match b with 
		h::t -> (listUnion (List.rev (h::(List.rev a ))) t)
		|[] -> a
	in
	let rec makeSubVocal str n =
		match str with 
		"0" -> []
		|"1"->
			if (n = 4) then ["천"]
			else if (n = 3 ) then ["백"]
			else if (n = 2 ) then ["십"]
			else if (n = 1) then ["일"]
			else []
		|_ -> 
			if (n = 4) then [ (matching str); "천"]
			else if ( n = 3) then [(matching str); "백"]
			else if (n = 2 ) then [(matching str); "십"]
			else if (n = 1) then [(matching str)]
			else []
	in 
	let rec makeVocal str =
		if ((str="000") || (str="0000")) then [ "영"]
		else if ((String.length str) > 0) then
		(listUnion (makeSubVocal (String.sub str 0 1) (String.length str)) (makeVocal (String.sub str 1 ((String.length str)-1))))
		else
		[]
	in
	if (((String.length strr) > 6 ) && ((String.length strr) < 9)) then
	[ (makeVocal (firstStr strr)); (makeVocal (lastStr strr))]
	else 
	raise (Error "invalid " )
	
