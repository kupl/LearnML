type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((e : aexp), (x : string)) : aexp =
  match e with
  | Const a -> Const 0
  | Var a -> if a = x then Const 1 else Const 0
  | Power (base, exp) ->
      if base = x then Times [ Const exp; Power (base, exp - 1) ] else Const 0
  | Times l -> (
      match l with
      | [] -> Const 0
      | __s63 :: __s64 ->
          Sum
            [
              Times (diff (__s63, x) :: __s64);
              Times [ __s63; diff (Times __s64, x) ];
            ]
      | [ _ ] -> Const 0
      | th :: tm :: tl ->
          Sum
            [
              Times [ diff (th, x); tm ];
              Times [ diff (tm, x); th ];
              diff (Times tl, x);
            ] )
  | Sum l -> (
      match l with
      | [] -> Const 0
      | __s66 :: __s67 -> Sum [ diff (__s66, x); diff (Sum __s67, x) ]
      | [ _ ] -> Const 0
      | th :: tm :: tl -> Sum [ diff (th, x); diff (Sum tl, x) ] )
