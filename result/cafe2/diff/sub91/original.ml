type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (var : string)) : aexp =
  match exp with
  | Const a -> Const 0
  | Var x -> if x = var then Const 1 else Var x
  | Power (x, set) ->
      if x = var then Times [ Const set; Power (x, set - 1) ] else Power (x, set)
  | Times l -> (
      match l with
      | [] -> Const 1
      | [ e ] -> diff (e, var)
      | hd :: tl ->
          Sum
            [ Times (diff (hd, var) :: tl); Times [ hd; diff (Times tl, var) ] ]
      )
  | Sum l -> (
      match l with
      | [] -> Const 0
      | [ e ] -> diff (e, var)
      | hd :: tl -> Sum [ diff (hd, var); diff (Sum tl, var) ] )
