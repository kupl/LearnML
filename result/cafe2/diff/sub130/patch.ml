type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((e : aexp), (x : string)) : aexp =
  match e with
  | Const n -> Const 0
  | Var y -> if y = x then Const 1 else Const 0
  | Power (x, 0) -> Const 0
  | Power (y, n) ->
      if y = x then Times [ Const n; Power (x, n - 1) ] else Const 0
  | Times [ Const n; Var y ] -> if y = x then Const n else Const 0
  | Times [ Const k; Power (y, n) ] ->
      if y = x then Times [ Const k; Const n; Power (x, n - 1) ] else Const 0
  | Times [ Power (y, p); Power (x, n) ] ->
      if y = x then Times [ Const (p + n); Power (x, n + p - 1) ]
      else Times [ Const n; Power (y, p); Power (x, n - 1) ]
  | Sum [ Const n; Var y ] -> if y = x then Const 1 else Const 0
  | Sum [ Const k; Power (y, n) ] ->
      if y = x then Times [ Const n; Power (x, n - 1) ] else Const 0
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
