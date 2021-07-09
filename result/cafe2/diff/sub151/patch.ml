type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((e : aexp), (x : string)) : aexp =
  match e with
  | Const c -> Const 0
  | Var c -> if c != x then Const 0 else Const 1
  | Power (c, d) ->
      if c != x then Const 0 else Times [ Const d; Power (c, d - 1) ]
  | Sum __s65 -> (
      match __s65 with
      | [] -> Const 0
      | __s66 :: __s67 -> Sum [ diff (__s66, x); diff (Sum __s67, x) ] )
  | Times __s62 -> (
      match __s62 with
      | [] -> Const 0
      | __s63 :: __s64 ->
          Sum
            [
              Times (diff (__s63, x) :: __s64);
              Times [ __s63; diff (Times __s64, x) ];
            ] )