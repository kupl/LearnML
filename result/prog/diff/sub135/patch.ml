type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((e : aexp), (x : string)) : aexp =
  match e with
  | Const y -> Const 0
  | Var s -> if s = x then Const 1 else Const 0
  | Times al -> (
      match al with
      | [] -> Const 0
      | hd :: tl ->
          Sum [ Times (diff (hd, x) :: tl); Times [ hd; diff (Times tl, x) ] ] )
  | Power (s, n) ->
      if s = x then Times [ Const n; Power (s, n - 1) ]
      else Times [ Power (s, n); Const 0 ]
  | Sum m -> (
      match m with
      | [] -> Const 0
      | hd :: tl -> Sum [ diff (hd, x); diff (Sum tl, x) ] )
