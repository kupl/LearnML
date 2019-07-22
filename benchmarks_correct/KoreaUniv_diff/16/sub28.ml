
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with
	| Const n -> Const 0
	| Var a -> if a = var then Const 1
						 else Const 0
	| Power (a, n) -> 
	begin 
		match n with
		| 0 -> Const 0
		| 1 -> if a = var then Const 1
					 else Const 0
		| 2 -> if a = var then Times [Const 2; Var a]
					 else Const 0
		| _ -> if a = var then Times [Const n; Power (a, n-1)]
					 else Const 0
	end
	| Times lst ->
	begin 
		match lst with
		| [] -> Const 0
		| hd :: tl -> match tl with
			| [] -> diff (hd, var)
			| thd :: [] -> (Sum [Times[diff (hd, var); thd]; Times[hd; diff(thd, var)]])
			| _ -> (Sum [Times[diff (hd, var); Times tl]; Times[hd; diff(Times tl, var)]])
	end
	| Sum lst -> 
		match lst with
		| [] -> Const 0
		| hd :: tl -> match tl with
			| [] ->  diff (hd, var)
			| thd :: [] -> (Sum [diff (hd, var); diff (thd, var)])
			| _ -> Sum ([diff (hd, var); diff (Sum tl, var)]);;