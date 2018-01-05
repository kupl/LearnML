exception Error of string

let vocalize number =
	let rec vocalizeRec n printed =
		let order_map = [(4,"õ");(3,"��");(2,"��");(1,"")]
		in
			let number_map = [('1',"��");('2',"��");('3',"��");('4',"��");('5',"��");('6',"��");('7',"ĥ");('8',"��");('9',"��")]
			in
				if String.length n = 0 then (if printed then [] else ["��"])
				else if String.get n 0 = '0' then vocalizeRec (String.sub n 1 (String.length n-1)) printed
				else if String.get n 0 = '1' && String.length n > 1 then (List.assoc (String.length n) order_map)::(vocalizeRec (String.sub n 1 (String.length n-1)) true)
				else (List.assoc (String.get n 0) number_map)::(List.assoc (String.length n) order_map)::(vocalizeRec (String.sub n 1 (String.length n-1)) true)
	in
		let f x = String.length x > 0
		in
			if (String.length number>8 || String.length number<7) then raise (Error "Invalid argument")
			else [List.filter f (vocalizeRec (String.sub number 0 (String.length number-4)) false);List.filter f (vocalizeRec (String.sub number (String.length number-4) 4) false)]