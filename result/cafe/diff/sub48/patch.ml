type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Const a -> Const 0
  | Var a -> if a != x then Const 0 else Const 1
  | Power (a, b) ->
      if a != x then Const 0 else Times [ Const b; Power (a, b - 1) ]
  | Times a -> (
      match a with
      | [] -> Const 0
      | hd :: tl ->
          Sum [ Times (diff (hd, x) :: tl); Times [ hd; diff (Times tl, x) ] ] )
  | Sum a -> (
      match a with
      | hd :: tl -> Sum [ diff (hd, x); diff (Sum tl, x) ]
      | _ -> Const 0 )
