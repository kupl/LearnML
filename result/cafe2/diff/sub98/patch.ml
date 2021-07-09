exception NotImplemented

type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (var : string)) : aexp =
  match exp with
  | Const n -> Const 0
  | Var k -> if k != var then Const 0 else Const 1
  | Power (k, n) ->
      if k = var then Times [ Const n; Power (k, n - 1) ] else Const 0
  | Sum hd :: tl ->
      if tl = [] then diff (hd, var)
      else Sum [ diff (hd, var); diff (Sum tl, var) ]
  | Times __s62 -> (
      match __s62 with
      | [] -> Const 0
      | __s63 :: __s64 ->
          Sum
            [
              Times (diff (__s63, var) :: __s64);
              Times [ __s63; diff (Times __s64, var) ];
            ] )
  | _ -> raise NotImplemented
