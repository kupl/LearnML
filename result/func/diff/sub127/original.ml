type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((e : aexp), (x : string)) : aexp =
  match e with
  | Const a -> Const 0
  | Var v -> if v = x then Const 1 else Const 0
  | Power (v, i) ->
      if i > 0 then Times [ Const i; Power (v, i - 1) ] else Const 0
  | Times lst -> (
      match lst with
      | [] -> Const 0
      | [ hd ] -> diff (hd, x)
      | hd :: tl ->
          Sum [ Times (diff (hd, x) :: tl); Times [ hd; diff (Times tl, x) ] ] )
  | Sum lst -> (
      match lst with
      | [] -> Const 0
      | [ hd ] -> diff (hd, x)
      | hd :: tl -> Sum [ diff (hd, x); diff (Sum tl, x) ] )
