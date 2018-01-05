exception ERROR of string
  let vocalize t =
	let read s =
		match s with
		"1" -> ["일"]
		|"2" -> ["이"]
		|"3" -> ["삼"]
		|"4" -> ["사"]
		|"5" -> ["오"]
		|"6" -> ["육"]
		|"7" -> ["칠"]
		|"8" -> ["팔"]
		|"9" -> ["구"]
		|_ -> []
		
	in
	
	let checkone s t =
		if (String.sub t s 1) = "1" then []
		else (read (String.sub t s 1))
	in

	let checkzero s t =
		if (String.sub t s 1) = "0" then []
		else
		(match s with
		0 -> []
		|1 -> ["십"]
		|2 -> ["백"]
		|3 -> ["천"]
		|_ -> [])
	in

	let zero3 t x y =
		if (String.sub t x 3) = "000" then ["영"]
		else y
	in

	let zero4 t x y =
		if (String.sub t x 4) = "0000" then ["영"]
		else y
	in

	if (String.length t) = 7 then
		[zero3 t 0 (checkone 0 t)
		@ (checkzero 2 t)
		@ (read (String.sub t 1 1)) @ (checkzero 1 t)
		@ (read (String.sub t 2 1));
		zero4 t 3 (checkone 3 t) 
		@ (checkzero 3 t)
		@ (read (String.sub t 4 1)) @ (checkzero 2 t)
		@ (read (String.sub t 5 1)) @ (checkzero 1 t)
		@ (read (String.sub t 6 1))]
	else if (String.length t) = 8 then
		[zero3 t 0 (checkone 0 t)
		@(checkzero 3 t)
		@(read (String.sub t 1 1))@(checkzero 2 t)
		@(read (String.sub t 2 1))@(checkzero 1 t)
		@(read (String.sub t 3 1));
		zero4 t 4 (checkone 4 t)
		@(checkzero 3 t)
		@(read (String.sub t 5 1))@(checkzero 2 t)
		@(read (String.sub t 6 1))@(checkzero 1 t)
		@(read (String.sub t 7 1))]
	else raise (ERROR "invalid input")


