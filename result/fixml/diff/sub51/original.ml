type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp =
 fun (aexp, x) ->
  match aexp with
  | Power (a, b) -> Times [ Const b; Var a ]
  | Const n -> Const 0
  | Var n -> Var n
  | Times hd :: tl ->
      Sum [ Times (diff (hd, x) :: tl); Times [ hd; diff (Times tl, x) ] ]
  | Times [] -> Const 0
  | Sum hd :: tl -> Sum [ diff (hd, x); diff (Sum tl, x) ]
  | Sum [] -> Const 0
