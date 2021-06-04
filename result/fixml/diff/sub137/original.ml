type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp =
 fun (e, x) ->
  match e with
  | Power (s, i) ->
      if s = x then Times [ Const i; Power (s, i - 1) ] else Const 1
  | Const i -> Const 0
  | Var s -> if s = x then Const 1 else Const 0
  | Sum [] -> Const 0
  | Sum hd :: tl -> Sum [ diff (hd, x); diff (Sum tl, x) ]
  | Times [] -> Const 1
  | Times hd :: tl ->
      Sum [ Times (diff (hd, x) :: tl); Times [ hd; diff (Times tl, x) ] ]
