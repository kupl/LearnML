exception Error
let number = [("0", "영");("1","일");("2","이");("3","삼");("4","사");("5","오");("6","육");("7","칠");("8","팔");("9","구")]
let unit_1 = [(4,"천");(3,"백");(2,"십")]

let vocalize a =
	let rec vocalize_sub a i =
	if i = 1 then (if (String.sub a 0 1)="0" then [] 
					else [(List.assoc (String.sub a 0 1) number)])
	else ( 
		match (List.assoc (String.sub a 0 1) number) with 
		"영" -> (vocalize_sub (String.sub a 1 (i-1)) (i-1))
		| "일" -> (List.append [(List.assoc i unit_1)] (vocalize_sub (String.sub a 1 (i-1)) (i-1)))
		| _ -> List.append ((List.assoc (String.sub a 0 1) number)::[(List.assoc i unit_1)]) (vocalize_sub (String.sub a 1 (i-1)) (i-1))) in
	let rec vocalize_first a i =
	if ((a="0000")||(a="000")) then ["영"] 
	else (vocalize_sub a i) in

 	match (String.length a) with
	7 -> List.append [(vocalize_first (String.sub a 0 3) 3)] [(vocalize_first (String.sub a 3 4) 4)]
	| 8 -> List.append [(vocalize_first (String.sub a 0 4) 4)] [(vocalize_first (String.sub a 4 4) 4)]
	| _ -> raise Error	
