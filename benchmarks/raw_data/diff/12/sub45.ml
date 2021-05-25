type aexp = Const of int
	| Var of string
	| Power of string * int
	| Times of aexp list
	| Sum of aexp list

let diff =
	let rec indiff = (fun (x, y) -> match x with
					| Const _ -> (Const 0)
					| Var a -> if a = y then (Const 1)
							else (Const 0)
					| Power (a, b) -> if a = y then Times [(Const b); (Power (a, b -1))]
							else (Const 0)
					| Sum x -> let rec sm = (fun x -> match x with
											| h:: t -> (indiff (h, y)):: (sm t)
											| _ -> []) in
							(Sum (sm x))
					| Times x -> let rec tms = (fun w x -> match x with
											| h:: t -> (Times (w@((indiff (h, y)):: t)))::(tms (w@[h]) t)
											| _ -> []
								) in (Sum (tms [] x))
		) in
	(fun (x, y) -> indiff(x, y))