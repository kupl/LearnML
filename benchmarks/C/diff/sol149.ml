
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list
  
  let rec map f l =
  match l with
  |[] -> []
  |h::t -> (f h)::(map f t)
  
  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with
  |Const x -> Const 0
  |Var x -> if x = var then Const 1 else Const 0
  |Power (str, n) -> if str = var then Times [Const n; Power (str, n - 1)] else Const 0
  |Times [] -> Const 0
  |Times (h::t) -> Sum [Times (diff (h,var)::t); Times [h; diff(Times t, var)]]
  |Sum x -> Sum (map (fun k -> diff (k, var)) x)
  