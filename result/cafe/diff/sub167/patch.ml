type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (x : string)) : aexp =
  match exp with
  | Times hd :: tl ->
      Sum [ Times (diff (hd, x) :: tl); Times [ hd; diff (Times tl, x) ] ]
  | Times [] -> Const 0
  | Sum hd :: tl -> Sum [ diff (hd, x); diff (Sum tl, x) ]
  | Sum [] -> Const 0
  | Const n -> Const 0
  | Var y -> if y = x then Const 1 else Const 0
  | Power (mit, jisu) ->
      if mit != x then Const 0 else Times [ Const jisu; Power (mit, jisu - 1) ]


let (_ : aexp) =
  diff (Sum [ Power ("x", 3); Times [ Const 2; Power ("x", 2) ] ], "x")


let (_ : aexp) =
  diff (Sum [ Power ("x", 2); Times [ Const 2; Var "x" ]; Const 1 ], "x")
