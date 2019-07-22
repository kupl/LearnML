type ae = CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list

let diff =
	let rec indiff = (fun (x, y) -> match x with
					| CONST _ -> (CONST 0)
					| VAR a -> if a = y then (CONST 1)
							else (CONST 0)
					| POWER (a, b) -> if a = y then TIMES [(CONST b); (POWER (a, b -1))]
							else (CONST 0)
					| SUM x -> let rec sm = (fun x -> match x with
											| h:: t -> (indiff (h, y)):: (sm t)
											| _ -> []) in
							(SUM (sm x))
					| TIMES x -> let rec tms = (fun w x -> match x with
											| h:: t -> (TIMES (w@((indiff (h, y)):: t)))::(tms (w@[h]) t)
											| _ -> []
								) in (SUM (tms [] x))
		) in
	(fun (x, y) -> indiff(x, y))