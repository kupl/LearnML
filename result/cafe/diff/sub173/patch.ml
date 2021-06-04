type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (x : string)) : aexp =
  match exp with
  | Const n -> Const 0
  | Var s -> if s = x then Const 1 else Const 0
  | Power (s, n) ->
      if s = x then Times [ Const n; Power (s, n - 1) ] else Const 0
  | Times lst -> (
      match lst with
      | [] -> Const 0
      | h :: t ->
          Sum [ Times (diff (h, x) :: t); Times [ h; diff (Times t, x) ] ] )
  | Sum lst -> (
      match lst with
      | h :: t ->
          if t = [] then diff (h, x) else Sum [ diff (h, x); diff (Sum t, x) ] )


let (_ : aexp) = Sum [ Power ("x", 2); Times [ Const 2; Var "x" ]; Const 1 ]

let (_ : aexp) =
  diff (Sum [ Power ("x", 2); Times [ Const 2; Var "x" ]; Const 1 ], "x")


let (_ : aexp) = diff (Sum [ Times [ Const 2; Var "x" ]; Const 2 ], "y")
