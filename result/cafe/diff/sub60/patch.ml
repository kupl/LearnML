type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Const a -> Const 0
  | Var y -> if x = y then Const 1 else Const 0
  | Power (y, a) ->
      if x != y then Const 0
      else if a = 1 then diff (Var y, x)
      else Times [ Const a; Power (y, a - 1) ]
  | Sum l -> (
      match l with
      | [] -> Const 0
      | h :: t -> Sum [ diff (h, x); diff (Sum t, x) ] )
  | Times __s62 -> (
      match __s62 with
      | [] -> Const 0
      | __s63 :: __s64 ->
          Sum
            [
              Times (diff (__s63, x) :: __s64);
              Times [ __s63; diff (Times __s64, x) ];
            ] )
