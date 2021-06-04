type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (var : string)) : aexp =
  match exp with
  | Const con -> Const 0
  | Var str -> if str = var then Const 1 else Const 0
  | Power (str, num) ->
      if str = var && num > 0 then Times [ Const num; Power (str, num - 1) ]
      else Const 0
  | Times time -> (
      match time with
      | [] -> Const 0
      | [ hd ] -> diff (hd, var)
      | hd :: tl ->
          Sum
            [ Times (diff (hd, var) :: tl); Times [ hd; diff (Times tl, var) ] ]
      )
  | Sum plus -> (
      match plus with
      | [] -> Const 0
      | [ hd ] -> diff (hd, var)
      | hd :: tl -> Sum [ diff (hd, var); diff (Sum tl, var) ] )
