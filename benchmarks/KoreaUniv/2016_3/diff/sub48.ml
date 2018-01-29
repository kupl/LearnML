
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> 
	match exp with
	| Const x -> Const 0;
	| Var v -> if var = v then Const 1 else Const 0
	| Power (v , i) -> if var = v then Times [Const i; Power(v, (i-1))]
																else Const 0
	| Times lt -> (match lt with | [] -> Const 0
| hd::tl -> Sum[(Times ((diff (hd, var))::tl)); Times [hd;(diff ((Times tl),var))]])
	| Sum lst ->
					 match lst with | [] -> Const 0
					| hd::tl -> Sum [(diff (hd, var)); (diff ((Sum tl), var))]