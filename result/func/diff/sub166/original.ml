type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec timehelp (lst : aexp list) : aexp =
  match lst with [] -> Times [] | hd :: tl -> Times [ hd; Times tl ]


let rec sumhelp (lst : aexp list) : aexp =
  match lst with [] -> Sum [] | hd :: tl -> Sum [ hd; Sum tl ]


let rec diff ((exp : aexp), (x : string)) : aexp =
  match exp with
  | Const a -> Const 0
  | Var x -> Const 1
  | Power (x, a) -> Times [ Const a; Power (x, a - 1) ]
  | Times [ a; b ] -> Sum [ Times [ diff (a, x); b ]; Times [ a; diff (b, x) ] ]
  | Times [ a ] -> diff (a, x)
  | Times [] -> Const 0
  | Times lst -> diff (timehelp lst, x)
  | Sum [ a; b ] -> Sum [ diff (a, x); diff (b, x) ]
  | Sum [ a ] -> diff (a, x)
  | Sum [] -> Const 0
  | Sum lst -> diff (sumhelp lst, x)


let (_ : aexp) = diff (Sum [ Times [ Const 2; Var "x" ]; Const 2 ], "x")

let (_ : aexp) =
  diff (Sum [ Power ("x", 2); Times [ Const 2; Var "x" ]; Const 1 ], "x")
