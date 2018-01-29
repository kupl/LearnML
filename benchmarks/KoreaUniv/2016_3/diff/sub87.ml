
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list




let rec diff : aexp * string -> aexp
  = fun (exp, var) -> 

match exp  with
|Var x -> if x=var then Const 1 else Const 0
|Const _ -> Const 0
|Power (x, a) -> if not(x=var) then Const 0
 								 else  Times [Const a; Power (var, a-1)]
|Sum lst -> Sum (List.map (fun y -> diff (y,var)) lst)
|Times [] -> Const 0
|Times (hd::tl) -> Sum [Times (diff (hd,var)::tl); Times [hd; diff(Times tl, var)]]
