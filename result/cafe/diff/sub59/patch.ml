type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Const c -> Const 0
  | Var v -> if v != x then Const 0 else Const 1
  | Power (p, a) ->
      if p != x then Const 0 else Times [ Const a; Power (p, a - 1) ]
  | Sum [] -> Const 0
  | Sum hd :: tl -> Sum [ diff (hd, x); diff (Sum tl, x) ]
  | Times __s62 -> (
      match __s62 with
      | [] -> Const 0
      | __s63 :: __s64 ->
          Sum
            [
              Times (diff (__s63, x) :: __s64);
              Times [ __s63; diff (Times __s64, x) ];
            ] )
