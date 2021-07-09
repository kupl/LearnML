type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Sum lst -> (
      match lst with
      | [] -> Const 0
      | hd :: tl -> Sum [ diff (hd, x); diff (Sum tl, x) ] )
  | Times lst -> (
      match lst with
      | [] -> Const 0
      | hd :: tl ->
          Sum
            [ Times [ diff (hd, x); mul tl ]; Times [ hd; diff (Times tl, x) ] ]
      )
  | Power (v, n) ->
      if v = x then Times [ Const n; Power (v, n - 1) ] else Const 0
  | Const n -> Const 0
  | Var y -> if Var y = Var x then Const 1 else Const 0


and mul (lst : aexp list) : aexp = if lst = [] then Const 1 else Times lst
