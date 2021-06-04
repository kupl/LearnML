type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp =
 fun (exp, x) ->
  match exp with
  | Const a -> Const 0
  | Var a -> if a = x then Const 1 else Const 0
  | Power (s, i) ->
      if s = x then Times [ Const i; Power (s, i - 1) ] else Const 0
  | Times l -> (
      match l with
      | [] -> Const 1
      | [ a ] -> diff (a, x)
      | h :: t -> Times [ h; diff (Times t, x) ] )
  | Sum m -> (
      match m with
      | [] -> Const 0
      | [ a ] -> diff (a, x)
      | h :: t -> Sum [ diff (h, x); diff (Sum t, x) ] )


let _ = diff (Sum [ Power ("x", 2); Times [ Const 2; Var "x" ]; Const 1 ], "x")
