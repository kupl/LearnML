
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list
  
  let rec cal_diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with
  | Const n -> Const 0
  | Var x -> if x=var then Const 1 else Const 0
  | Power (x,n) -> if x=var then (match n with
	|0 -> Const 0
	|1 -> Const 1
	|2 -> Times[Const 2; Var var]
	|_ -> Times[Const n; Power (var, n-1)])
	else Const 0
  | Times (hd :: tl) -> if tl = [] then (cal_diff (hd,var))
  else (Sum [(Times([cal_diff(hd,var)] @ tl));(Times([hd] @ [(cal_diff(Times tl,var))]))])
  | Sum (hd :: tl) -> if tl = [] then (cal_diff(hd,var)) else (Sum ([(cal_diff(hd,var))] @ [(cal_diff(Sum(tl),var))]))
  | Times [] -> Const 0
  | Sum [] -> Const 0
  
  let diff : aexp * string -> aexp
  = fun (exp, var) -> cal_diff (exp,var)