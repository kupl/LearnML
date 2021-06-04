type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp =
 fun (exp, var) ->
  match exp with
  | Const n -> Const 0
  | Var v -> if v = var then Const 1 else Const 0
  | Power (v, n) ->
      if v = var && n > 1 then Times [ Const n; Power (v, n - 1) ]
      else if n = 1 then Const 1
      else Const 0
  | Times lst -> (
      match lst with
      | [] -> Const 0
      | hd :: tl ->
          Sum
            [ Times (diff (hd, var) :: tl); Times [ hd; diff (Times tl, var) ] ]
      )
  | Sum lst -> (
      match lst with
      | [] -> Const 0
      | hd :: tl -> Sum [ diff (hd, var); diff (Sum tl, var) ] )
