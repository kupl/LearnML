type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (var : string)) : aexp =
  match exp with
  | Const n -> Const 0
  | Var x -> if x = var then Const 1 else Const 0
  | Power (x, n) ->
      if x = var then Times [ Const n; Power (x, n - 1) ] else Const 0
  | [ Times __s61 ] -> diff (__s61, var)
  | Times l -> (
      match l with
      | [] -> raise Failure "Times error"
      | h :: t ->
          Sum [ Times (diff (h, var) :: t); Times [ h; diff (Times t, var) ] ] )
  | [ Sum __s64 ] -> diff (__s64, var)
  | Sum l -> (
      match l with
      | [] -> raise Failure "Sum error"
      | h :: t -> Sum [ diff (h, var); diff (Sum t, var) ] )