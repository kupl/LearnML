
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> 
  		match exp with
  		| Const n -> Const 0
  		| Var v -> 
  			if v = var then Const 1
  			else Const 0
  		| Power (x, n) -> 
			if x = var then 
				if n = 0 then Const 0
				else if n = 1 then Const 1
				else Times [ Const n; Power (x, n-1) ]
			else Const 0
		| Times l ->
			(
				match l with
				| [] -> Const 0
				| hd::[] -> diff(hd, var) 
				| hd::tl -> 
					match hd with
					| Const 1 ->
					diff (Times tl, var)
					| Const n ->
						Times [ hd; diff ( (Times tl), var ) ]
					| _ -> 
						Sum [Times (diff(hd, var)::tl); Times [ hd; diff(Times tl, var)]]
				)
		| Sum l ->
			match l with
			| [] -> Const 0
			| hd::[] -> (diff (hd, var))
			| hd::tl -> Sum [(diff (hd, var)); (diff ((Sum tl),var))]