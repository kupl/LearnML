type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp =
 fun (exp, x) ->
  match exp with
  | Const n -> Const 0
  | Var s -> if s = x then Const 1 else Const 0
  | Power (s, n) ->
      if s = x then Times [ Const n; Power (s, n - 1) ] else Const 0
  | Times lst -> (
      match lst with
      | h :: t ->
          if t = [] then diff (h, x)
          else Sum [ Times (diff (h, x) :: t); Times [ h; diff (Sum t, x) ] ] )
  | Sum lst -> (
      match lst with
      | h :: t ->
          if t = [] then diff (h, x) else Sum [ diff (h, x); diff (Sum t, x) ] )


;;
Sum [ Power ("x", 2); Times [ Const 2; Var "x" ]; Const 1 ]

;;
diff (Sum [ Power ("x", 2); Times [ Const 2; Var "x" ]; Const 1 ], "x")

;;
diff (Sum [ Times [ Const 2; Var "x" ]; Const 2 ], "y")
