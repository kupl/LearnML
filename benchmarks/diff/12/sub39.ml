type ae = CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list


let rec diff (a, s) =
	let rec timesf l n =
		let timesfn l n = 
			let rec cutlistf l n =
			        if (n = 0) then []
			        else (List.hd l) :: (cutlistf (List.tl l) (n - 1))
			in

			let rec cutlistl l n =
			        if (n = 0) then (List.tl l)
			        else (cutlistl (List.tl l) (n - 1))
			in
		
			(TIMES ((cutlistf l n) @ [(diff ((List.nth l n), s))] @ (cutlistl l n)))
		in

		if (n = (List.length l)) then []
		else (timesfn l n) :: (timesf l (n + 1))
	in
		
	let diffs na = (diff (na, s)) in

	match a with
	| CONST n -> (CONST 0)
	| VAR ms -> if (ms = s) then (CONST 1)
			else (CONST 0)
	| POWER (ms, n) -> if (ms = s) then (TIMES [(CONST n); (POWER (ms, (n - 1)))])
			else (CONST 0)
	| TIMES l -> (SUM (timesf l 0))
	| SUM l -> (SUM (List.map diffs l))
	
