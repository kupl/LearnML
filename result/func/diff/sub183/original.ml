type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (x : string)) : aexp =
  match exp with
  | Const c -> Const 0
  | Power (str, 0) -> Const 1
  | Power (str, 1) -> Var str
  | Power (str, n) -> Times [ Const n; Power (str, n - 1) ]
  | Times [ Const n; Var str ] -> Const n
  | Times [ Const n; ex ] -> Times [ Const n; diff (ex, x) ]
  | Times [ ex; Var str ] -> Times [ diff (ex, x); Var str ]
  | Times [ ex1; ex2 ] -> Times [ diff (ex1, x); diff (ex2, x) ]
  | Sum [ Const n ] -> Const n
  | Sum [ hd; tl ] -> Sum [ diff (hd, x); diff (tl, x) ]
  | Sum [ h1; h2; tl ] -> Sum [ diff (h1, x); diff (h2, x); diff (tl, x) ]
