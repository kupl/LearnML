type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Const n -> Const 0
  | Var chars -> if chars != x then Const 0 else Const 1
  | Power (chars, n) ->
      if chars != x then Const 0 else Times [ Const n; Power (chars, n - 1) ]
  | Times lst -> (
      match lst with
      | [] -> Const 0
      | aexp :: tl ->
          Sum
            [ Times (diff (aexp, x) :: tl); Times [ aexp; diff (Times tl, x) ] ]
      )
  | Sum lst -> (
      match lst with
      | [] -> Const 0
      | hd :: tl -> Sum [ diff (hd, x); diff (Sum tl, x) ] )
