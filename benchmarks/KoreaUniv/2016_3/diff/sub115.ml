
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with
	| Const n -> Const 0
	| Var v -> if v = var then Const 1 else Const 0
	| Power (v, n) -> if v = var then Times [Const n; Power (var, n-1)] else Const 0
	| Times [] -> Const 0
	| Times [a] -> diff (a, var) (*For Simpler Result*)
	| Times (hd::tl) -> Sum [Times (diff (hd, var)::tl) ; Times [hd ; diff (Times tl, var)]]
	| Sum [] -> Const 0
	| Sum [a] -> diff (a,var) (*For Simpler Result*)
	| Sum (hd::tl) -> Sum [diff (hd,var) ; diff (Sum tl,var)]