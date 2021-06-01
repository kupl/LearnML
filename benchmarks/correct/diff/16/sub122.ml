
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> 
    match exp with
      | Const a -> Const 0
      | Var a -> if var = a then Const 1 else Const 0
      | Power (a, b) -> 
          (if a <> var then Const 0
          else (Times[Const b; Power(a, b-1)]))
      | Times [] -> Const 0
      | Times (hd::tl) -> Sum [Times (diff (hd, var)::tl); Times [hd; diff(Times tl, var)]]
      | Sum a -> Sum (List.map (fun ae -> diff (ae,var)) a)