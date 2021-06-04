type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Const _ -> Const 0
  | Const c -> Const c
  | Var v -> if x = v then Const 1 else Const 0
  | Power (p, a) ->
      if not (x = p) then Const 0 else Times [ Const a; Power (p, a - 1) ]
  | Times [] -> Const 0
  | Times hd :: tl ->
      Sum [ Times (diff (hd, x) :: tl); Times [ hd; diff (Times tl, x) ] ]
  | Sum [] -> Const 0
  | Sum hd :: tl -> Sum [ diff (hd, x); diff (Sum tl, x) ]
