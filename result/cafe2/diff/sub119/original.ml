type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let diff ((e : aexp), (x : string)) : aexp =
  let rec helper ((e : aexp), (x : string)) : aexp =
    match e with
    | Const a -> Const 0
    | Var v -> if v = x then Const 1 else Const 0
    | Power (s, i) ->
        if s = x then Times [ Const i; Power (s, i - 1) ] else Const 0
  in
  helper (e, x)