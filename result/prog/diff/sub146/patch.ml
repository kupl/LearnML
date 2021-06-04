type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((e : aexp), (x : string)) : aexp =
  match e with
  | Const i -> Const 0
  | Var s -> if s = x then Const 1 else Const 0
  | Power (s, i) ->
      if s = x then Times [ Const i; Power (s, i - 1) ] else Const 0
  | Times __s62 :: __s63 ->
      Sum
        [
          Times (diff (__s62, x) :: __s63);
          Times [ __s62; diff (Times __s63, x) ];
        ]
  | Times l -> (
      match l with
      | [] -> Const 0
      | hd :: tl ->
          Sum
            [ Times (diff (hd, "x") :: tl); Times [ hd; diff (Times tl, "x") ] ]
      )
  | Sum __s64 :: __s65 -> Sum [ diff (__s64, x); diff (Sum __s65, x) ]
  | Sum l -> (
      match l with
      | [] -> Const 0
      | hd :: tl -> Sum [ diff (hd, "x"); diff (Sum tl, "x") ] )
