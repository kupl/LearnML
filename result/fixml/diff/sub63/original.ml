type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp =
 fun (aexp, x) ->
  match aexp with
  | Const i -> Const 0
  | Var k -> if k = x then Const 1 else Var k
  | Power (k, i) ->
      if k = x then Times [ Const i; Power (k, i - 1) ] else Power (k, i)
  | Times l -> (
      match l with
      | [] -> Const 0
      | h :: t -> (
          match h with
          | Var k ->
              if k = x then Times (diff (h, x) :: t)
              else Times [ h; diff (Times t, x) ]
          | Power (k, i) ->
              if k = x then Times (diff (h, x) :: t)
              else Times [ h; diff (Times t, x) ]
          | _ -> Times [ h; diff (Times t, x) ] ) )
  | Sum l -> (
      match l with
      | [] -> Const 0
      | h :: t -> Sum [ diff (h, x); diff (Sum t, x) ] )
