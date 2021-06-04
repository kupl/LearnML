type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((e : aexp), (x : string)) : aexp =
  match e with
  | Const n -> Const 0
  | Var y -> if y = x then Const 1 else Const 0
  | Power (y, i) ->
      if y = x then Times [ Const i; Power (y, i - 1) ] else Const 0
  | Times [ Const n; Const m ] -> Const 0
  | Times [ Const n; Var y ] -> if y = x then Const n else Const 0
  | Times [ Var y; Const n ] -> if y = x then Const n else Const 0
  | Times [ Const n; Power (y, i) ] ->
      if y = x then Times [ Const n; diff (Power (y, i), x) ] else Const 0
  | Times [ Power (y, i); Const n ] ->
      if y = x then Times [ Const n; diff (Power (y, i), x) ] else Const 0
  | Times [ Const n; Times l ] -> Times [ Const n; diff (Times l, x) ]
  | Times [ Times l; Const n ] -> Times [ Const n; diff (Times l, x) ]
  | Times [ Const n; Sum l ] -> Times [ Const n; diff (Sum l, x) ]
  | Times [ Sum l; Const n ] -> Times [ Const n; diff (Sum l, x) ]
  | Sum hd :: tl -> Sum [ diff (hd, x); diff (Sum tl, x) ]
  | _ -> Const 0
