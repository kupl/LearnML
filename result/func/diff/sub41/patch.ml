type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Const a -> Const 0
  | Var a -> if a = x then Const 1 else Const 0
  | Power (a, b) ->
      if a = x then Times [ Const b; Power (a, b - 1) ] else Const 0
  | Times l -> (
      match l with
      | [] -> Const 0
      | [ hd ] -> diff (hd, x)
      | hd :: tl ->
          Sum [ Times (diff (hd, x) :: tl); Times [ hd; diff (Times tl, x) ] ] )
  | Sum l ->
      let iterSum (a : aexp) : aexp = diff (a, x) in
      Sum (List.map iterSum l)
