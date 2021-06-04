type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (var : string)) : aexp =
  match exp with
  | Const c -> Const 0
  | Var s -> if s = var then Const 1 else Const 0
  | Power (s, n) -> (
      if s != var then Const 0
      else
        match n with
        | 1 -> Times [ Const 1 ]
        | _ -> Times [ Const n; Power (s, n - 1) ] )
  | Times l -> (
      match l with
      | [] -> Const 0
      | hd :: tl ->
          Sum
            [ Times (diff (hd, var) :: tl); Times [ hd; diff (Times tl, var) ] ]
      )
  | Sum __s65 -> (
      match __s65 with
      | [] -> Const 0
      | __s66 :: __s67 -> Sum [ diff (__s66, var); diff (Sum __s67, var) ] )
