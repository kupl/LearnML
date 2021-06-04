type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (env : string)) : aexp =
  match exp with
  | Const a -> Const 0
  | Var x -> if x != env then Const 0 else Const 1
  | Power (s, i) ->
      if s != env then Const 0 else Times [ Const i; Power (s, i - 1) ]
  | Sum __s65 -> (
      match __s65 with
      | [] -> Const 0
      | __s66 :: __s67 -> Sum [ diff (__s66, env); diff (Sum __s67, env) ] )
  | Times __s62 -> (
      match __s62 with
      | [] -> Const 0
      | __s63 :: __s64 ->
          Sum
            [
              Times (diff (__s63, env) :: __s64);
              Times [ __s63; diff (Times __s64, env) ];
            ] )
