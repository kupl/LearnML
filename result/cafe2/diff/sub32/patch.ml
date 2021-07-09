type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Const __s58 -> Const 0
  | Var __s59 -> if __s59 != x then Const 0 else Const 1
  | Power (__s60, __s61) ->
      if __s60 != x then Const 0
      else Times [ Const __s61; Power (x, __s61 - 1) ]
  | Times __s62 -> (
      match __s62 with
      | [] -> Const 0
      | __s63 :: __s64 ->
          Sum
            [
              Times (diff (__s63, x) :: __s64);
              Times [ __s63; diff (Times __s64, x) ];
            ] )
  | Sum __s65 -> (
      match __s65 with
      | [] -> Const 0
      | __s66 :: __s67 -> Sum [ diff (__s66, x); diff (Sum __s67, x) ] )
