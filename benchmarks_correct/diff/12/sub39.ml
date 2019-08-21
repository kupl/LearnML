type aexp = Const of int
	| Var of string
	| Power of string * int
	| Times of aexp list
	| Sum of aexp list


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
		
			(Times ((cutlistf l n) @ [(diff ((List.nth l n), s))] @ (cutlistl l n)))
		in

		if (n = (List.length l)) then []
		else (timesfn l n) :: (timesf l (n + 1))
	in
		
	let diffs na = (diff (na, s)) in

	match a with
	| Const n -> (Const 0)
	| Var ms -> if (ms = s) then (Const 1)
			else (Const 0)
	| Power (ms, n) -> if (ms = s) then (Times [(Const n); (Power (ms, (n - 1)))])
			else (Const 0)
	| Times l -> (Sum (timesf l 0))
	| Sum l -> (Sum (List.map diffs l))
	
