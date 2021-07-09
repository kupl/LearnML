type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Const i -> Const 0
  | Var s -> if s = x then Const 1 else Const 0
  | Power (s, i) ->
      if s != x then Const 0 else Times [ Const i; Power (s, i - 1) ]
  | Times lst -> (
      match lst with
      | [] -> Const 0
      | hd :: tl ->
          Sum
            ( [ Times (diff (hd, x) :: tl) ]
            @ [ Times [ hd; diff (Times tl, x) ] ] ) )
  | Sum lst -> (
      match lst with
      | [] -> Const 0
      | hd :: tl -> Sum ([ diff (hd, x) ] @ [ diff (Sum tl, x) ]) )
