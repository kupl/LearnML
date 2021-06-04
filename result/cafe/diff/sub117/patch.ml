type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((e : aexp), (x : string)) : aexp =
  match e with
  | Const a -> Const 0
  | Var y -> if y != x then Const 0 else Const 1
  | Power (s, i) ->
      if s != x then Const 0 else Times [ Const i; Power (s, i - 1) ]
  | Sum m -> (
      match m with
      | [] -> Const 0
      | __s66 :: __s67 -> Sum [ diff (__s66, x); diff (Sum __s67, x) ] )
  | Times l -> (
      match l with
      | [] -> Const 0
      | __s63 :: __s64 ->
          Sum
            [
              Times (diff (__s63, x) :: __s64);
              Times [ __s63; diff (Times __s64, x) ];
            ] )
