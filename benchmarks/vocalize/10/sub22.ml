exception Error of string
let rec vocalize a =
	let length = String.length a in 
	let lm1 = length - 1 in 
	let lm4 = length - 4 in 
	let lm5 = length - 5 in 
	let zero = "0000" in
	let zero1 = "000" in 
	let read n = 
	match n with
	 '1' -> ["일"]
	| '2' -> ["이"]
	| '3' -> ["삼"]
	| '4' -> ["사"]
	| '5' -> ["오"]
	| '6' -> ["육"]
	| '7' -> ["칠"]
	| '8' -> ["팔"]
	| '9' -> ["구"]
	| _ -> []
	in 
	let rec vocal1 index =
	if length = 7 & String.sub a 0 3 = zero1 then ["영"]
	else if length = 8 & String.sub a 0 4 = zero then ["영"] 
	else if index = lm4 then []
	else if a.[lm5 - index] = '0' then vocal1 (index + 1)  
	else match index mod 4 with
		 0 -> vocal1 (index + 1) @ read a.[lm5 - index] 
		| 1 -> vocal1 (index + 1) @ if a.[lm5 - index] = '1' then ["십"] else read a.[lm5 - index] @ ["십"]
		| 2 -> vocal1 (index + 1) @ if a.[lm5 - index] = '1' then ["백"] else read a.[lm5 - index] @ ["백"] 
		| 3 -> if a.[lm5 - index] = '1' then ["천"] else read a.[lm5 - index] @ ["천"] 
		| _ -> []
	in
	let rec vocal2 index =
	if String.sub a lm4 4 = zero then ["영"]
	else if a.[lm1 - index] = '0' & index = 3 then []
	else if a.[lm1 - index] = '0' then vocal2 (index + 1)
	else match index mod 4 with
		0 -> vocal2 (index + 1) @ read a.[lm1 - index] 
		| 1 -> vocal2 (index + 1) @ if a.[lm1 - index] = '1' then ["십"] else read a.[lm1 - index] @ ["십"] 
		| 2 -> vocal2 (index + 1) @ if a.[lm1 - index] = '1' then ["백"] else read a.[lm1 - index] @ ["백"] 
		| 3 -> if a.[lm1 - index] = '1' then ["천"] else read a.[lm1 - index] @ ["천"] 
		| _ -> []
	in
	if length < 7 or length > 8 then raise (Error "invalid input!")
	else [vocal1 0; vocal2 0];;
	     




