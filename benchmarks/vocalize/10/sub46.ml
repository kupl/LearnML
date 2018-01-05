exception ERROR
let vocalize str =
	let numvo n =
		match n with
		'0' -> []
		| '1' -> []
		| '2' -> ["이"]
		| '3' -> ["삼"]
		| '4' -> ["사"]
		| '5' -> ["오"]
		| '6' -> ["육"]
		| '7' -> ["칠"]
		| '8' -> ["팔"]
		| '9' -> ["구"]
		| _ -> raise ERROR  
	in

	let civo nth =
		match nth with	
		1 -> []
		| 2 -> ["십"]
		| 3 -> ["백"]
		| 4 -> ["천"]
		| _ -> raise ERROR
	in

	let rec vocal s = 
		if (String.length s) = 0 
			then []
			else (numvo (String.get s 0))@
				(civo (String.length s))@
				(vocal (String.sub s 1 ((String.length s)-1)))
	in
	let e = String.length str in
	let m = 
		if e = 7 
			then 3 else 4 in
	[(vocal (String.sub str 0 m))@
	(if (String.get str (m-1)) = '1' then ["일"] else []);
	(vocal (String.sub str m 4))@
	(if (String.get str (e-1)) = '1' then ["일"] else [])]



