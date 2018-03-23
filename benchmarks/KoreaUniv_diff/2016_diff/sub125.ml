
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> 
    match exp with
      |Const a -> Const 0
      |Var "x" -> Const 1
      |Power("x", a) 
        -> if "x" = var then Times [Const a; Power("x", a-1)] 
           else Const 0;;