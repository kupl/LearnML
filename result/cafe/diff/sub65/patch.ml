type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Const a -> Const 0
  | Var b -> if b != x then Const 0 else Const 1
  | Power (b, a) ->
      if b != x then Const 0 else Times [ Const a; Power (b, a - 1) ]
  | Times l -> (
      match l with
      | [] -> Const 0
      | hd :: tl ->
          Sum [ Times (diff (hd, x) :: tl); Times [ hd; diff (Times tl, x) ] ] )
  | Sum l -> (
      match l with
      | [] -> Const 0
      | hd :: tl -> Sum [ diff (hd, x); diff (Sum tl, x) ] )
