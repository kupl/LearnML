type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (x : string)) : aexp =
  match exp with
  | Const c -> Const 0
  | Var m -> if m = x then Const 1 else Const 0
  | Power (m, c) ->
      if m = x then Times [ Const c; Power (m, c - 1) ] else Const 0
  | Times m -> (
      match m with
      | [] -> Const 0
      | hd :: tl ->
          Sum
            [
              Times [ diff (hd, x); Times tl ]; Times [ hd; diff (Times tl, x) ];
            ] )
  | Sum m -> (
      match m with
      | [] -> Const 0
      | hd :: tl -> Sum [ diff (hd, x); diff (Sum tl, x) ] )


let (_ : aexp) =
  diff (Sum [ Power ("x", 2); Times [ Const 2; Var "x" ]; Const 1 ], "x")
