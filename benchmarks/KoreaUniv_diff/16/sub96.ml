
exception NotImplemented

  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with
	| Const a -> Const 0
	| Var a -> if a = var then Const 1 else Const 0
	| Power (a, b) -> if a <> var || b = 0 then Const 0 else if b = 1 then Const 1 else if b = 2 then Times [Const b; Var a] else Times [Const b; Power (a, b - 1)]
	| Times a -> (match a with
		| [] -> raise NotImplemented
		| [hd] -> diff (hd, var)
		| hd::tl -> if diff (hd, var) = Const 0 then Times ([hd]@[diff (Times tl, var)]) else Sum [Times ([diff(hd, var)]@tl); Times ([hd]@[diff (Times tl, var)])])
	| Sum a -> (match a with
		| [] -> raise NotImplemented
		| [hd] -> diff (hd, var)
		| hd::tl -> if diff (hd, var) = Const 0 then diff (Sum tl, var) else (match diff(Sum tl, var) with
			| Sum b -> Sum ([diff(hd, var)]@b)
			| Const 0 -> Sum ([diff (hd, var)])
			| _ -> Sum ([diff (hd, var)]@[diff(Sum tl, var)])))