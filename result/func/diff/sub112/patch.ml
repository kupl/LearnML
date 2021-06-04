type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (var : string)) : aexp =
  match exp with
  | Sum a -> (
      match a with
      | hd :: tl -> Sum [ diff (hd, var); diff (Sum tl, var) ]
      | _ -> Const 0 )
  | Times a -> (
      match a with
      | hd :: tl ->
          Sum
            [ Times (diff (hd, var) :: tl); Times [ hd; diff (Times tl, var) ] ]
      | _ -> Const 0 )
  | Power (a, b) ->
      if a = var then Times [ Power (a, b - 1); Const b ] else Const 0
  | Var a -> if a = var then Const 1 else Const 0
  | Const a -> Const 0
