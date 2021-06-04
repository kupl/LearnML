type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((e : aexp), (x : string)) : aexp =
  match e with
  | Const integer -> Const 0
  | Var str -> if str = x then Const 1 else Const 0
  | Power (str, integer) ->
      if str = x then Times [ Const integer; Power (str, integer - 1) ]
      else Const 0
  | Times al1 -> (
      match al1 with
      | [] -> Const 0
      | [ aexp1 ] -> diff (aexp1, x)
      | h :: t ->
          Sum [ Times [ diff (h, x); Times t ]; Times [ h; diff (Times t, x) ] ]
      )
  | Sum al2 -> (
      match al2 with
      | [] -> Const 0
      | [ aexp1 ] -> diff (aexp1, x)
      | h :: t -> Sum [ diff (h, x); diff (Sum t, x) ] )
