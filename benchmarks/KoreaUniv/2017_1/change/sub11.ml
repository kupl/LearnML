(* problem 8*)

let change : int list -> int -> int
= fun coins amount -> (*TODO*)
	let rec help c m a =
		if a == 0 then 1
		else if a < 0 then 0
		else if c == [] then 0
		else if (m <= 0 && a >= 1) then 1
		else (help c (m-1) a + help c m (a-(List.nth c ((List.length c)-1)))) in
			help coins (List.length coins) amount

