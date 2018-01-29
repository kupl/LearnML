
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> 
		match exp with 
		|Const x -> Const 0
		|Var x -> if var = x then Const 1 else Const 0
		|Power (x,y)->
			(if x <> var then Const 0
			else (Times[Const y; Power(x, y-1)]))
		|Times [] -> Const 0
		|Times (hd::tl) -> Sum [Times(diff(hd, var)::tl); Times [hd; diff(Times tl, var)]]
		|Sum x -> Sum (List.map(fun ae -> diff(ae,var)) x)