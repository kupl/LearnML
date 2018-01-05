exception Error of string

let rec read n =
	let rec readA n = 
		let num = ["��"; "��"; "��"; "��"; "��"; "��"; "��"; "ĥ"; "��"; "��"] in

		if n < 10 then
			if n != 0 then [List.nth num n]
			else []
		else if n < 100 then
			let c = n / 10 mod 10 in
			let next = readA (n mod 10) in
			if c = 0 then next
			else if c = 1 then "��"::next
			else (List.nth num c)::"��"::next
		else if n < 1000 then
			let c = n / 100 mod 10 in
			let next = readA (n mod 100) in
			if c = 0 then next
			else if c = 1 then "��"::next
			else (List.nth num c)::"��"::next
		else
			let c = n / 1000 mod 10 in
			let next = readA (n mod 1000) in
			if c = 0 then next
			else if c = 1 then "õ"::next
			else (List.nth num c)::"õ"::next
	in
	
	if n = 0 then
		["��"]
	else
		readA n

let vocalize str =
	if String.length str <= 6 then
		raise (Error "length error")
	else if String.length str >= 9 then
		raise (Error "length error")
    else if String.length str = 7 then
        [read (int_of_string (String.sub str 0 3)); read (int_of_string (String.sub str 3 4))]
    else
        [read (int_of_string (String.sub str 0 4)); read (int_of_string (String.sub str 4 4))]