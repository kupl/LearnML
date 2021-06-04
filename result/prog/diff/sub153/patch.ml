type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (x : string)) : aexp =
  match exp with
  | Const a -> Const 0
  | Var y -> if y = x then Const 1 else Const 0
  | Power (y, a) ->
      if y = x then Times [ Const a; Power (y, a - 1) ] else Const 0
  | Sum lst ->
      let rec difflst ((lst' : aexp list), (x' : string)) : aexp list =
        match lst' with
        | [] -> []
        | hd :: tl -> diff (hd, x') :: difflst (tl, x')
      in
      Sum (difflst (lst, x))
  | Times lst -> (
      match lst with
      | [] -> Times [ Const 0 ]
      | hd :: tl ->
          let rec difflst ((lst' : aexp list), (x' : string)) : aexp list =
            match lst' with [] -> [] | hd' :: tl' -> diff (hd', x') :: tl'
          in
          Sum [ Times (difflst (lst, x)); Times [ hd; diff (Times tl, x) ] ] )


let (_ : aexp) =
  diff (Sum [ Power ("x", 2); Times [ Const 2; Var "x" ]; Const 1 ], "x")
