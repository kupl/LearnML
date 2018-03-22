
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
	| Times of aexp list
	| Sum of aexp list


let rec diff (exp, var) =
	match exp with
	| Const a -> Const 0
	| Var x when x = var -> Const 1
	| Var y -> Const 0
	| Power (str, a) ->
		(
		if str = var then
			(match a with
			| 0 -> Const 0
			| 1 -> Const 1
			| 2 -> Times [Const 2; Var var]
			| _ -> let aa = a-1 in Times [Const a ; Power (str, aa)]
			)
		else Const 0
		) 
	| Times ls ->
		(
		match ls with
		| [] -> Const 0
		| [hd] -> diff (hd, var)
		| hd :: [tl] -> if hd = Const 0 || tl = Const 0 then Const 0
										else Sum [Times [diff (hd, var); tl]; Times [hd; diff (tl, var)]]
		| hd :: tl -> if hd = Const 0  then Const 0
									else Sum [Times [diff (hd, var); Times tl]; Times [hd; diff (Times tl, var)]]
		)
	| Sum ls ->
		(
		match ls with
		| [] -> Const 0
		| [hd] -> diff (hd, var)
		| hd :: [tl] -> if hd = Const 0 then diff (tl, var)
										else if tl = Const 0 then diff (hd, var)
										else Sum [diff (hd, var); diff (tl, var)]
		| hd :: tl -> if hd = Const 0 then diff (Sum tl, var)
									else Sum [diff (hd, var); diff (Sum tl, var)]
		)
