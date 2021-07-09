type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (x : string)) : aexp =
  match exp with
  | Const n -> Const 0
  | Var s -> if s != x then Const 0 else Const 1
  | Power (str, pow) ->
      if str != x then Const 0 else Times [ Const pow; Power (str, pow - 1) ]
  | Times lst -> (
      match lst with
      | [] -> Const 0
      | hd :: tl ->
          if tl != [] then
            Sum
              [
                Times [ diff (hd, x); Times tl ];
                Times [ hd; diff (Times tl, x) ];
              ]
          else diff (hd, x) )
  | Sum lst -> (
      match lst with
      | [] -> Const 0
      | hd :: tl ->
          if tl != [] then Sum [ diff (hd, x); diff (Sum tl, x) ]
          else diff (hd, x) )
