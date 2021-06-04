type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (x : string)) : aexp =
  match exp with
  | Const n -> Const 0
  | Var st -> if st = x then Const 1 else Const 0
  | Power (st, n) ->
      if st = x then
        if n = 2 then Times [ Const n; Var st ]
        else Times [ Const n; Power (st, n - 1) ]
      else Const 0
  | Times [ e ] -> diff (e, x)
  | Times hd :: tl ->
      Sum [ Times (diff (hd, x) :: tl); Times [ hd; diff (Times tl, x) ] ]
  | Times [] -> Const 1
  | Sum [ e ] -> diff (e, x)
  | Sum hd :: tl -> Sum [ diff (hd, x); diff (Sum tl, x) ]
  | Sum [] -> Const 0
