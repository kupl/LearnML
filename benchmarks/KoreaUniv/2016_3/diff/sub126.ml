
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with
	| Const(a) -> Const 0
	| Var(x) -> if (x=var) then Const 1 else Const 0
	| Power(s,a) -> if (s=var) then Times[Const a; Power(s,a-1)] else Const 0
	| Times lst -> (match lst with
		| [] -> Const 0
		| hd::tl -> Sum[(Times(diff(hd, var)::tl));(Times[hd; diff(Times tl, var)])]
		)
	| Sum lst -> (match lst with
		| [] -> Const 0
		| hd::tl -> Sum[(diff(hd,var));(diff(Sum tl, var))]
		)