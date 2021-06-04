type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (x : string)) : aexp =
  match exp with
  | Const n -> Const 0
  | Times [ Const n; Var x ] -> Var x
  | Power (x, n) -> Times [ Const n; Power (x, n - 1) ]
  | Sum [ hd; tl ] -> diff (Sum [ exp ], x)
