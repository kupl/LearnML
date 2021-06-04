type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp =
 fun (exp, x) ->
  match exp with
  | Const n -> Const 0
  | Var y -> if y = x then Const 1 else Const 0
  | Power (y, n) ->
      if n != 0 && y = x then Times [ Const n; Power (y, n - 1) ] else Const 0
  | Times lst -> (
      match lst with
      | [] -> Const 0
      | hd :: tl -> (
          match hd with
          | Var x -> if tl = [] then Const 1 else Times tl
          | Power (x, n) ->
              if n != 0 then Times [ Power (x, n - 1); Times tl ]
              else diff (Times tl, x)
          | _ -> Times [ hd; diff (Times tl, x) ] ) )
  | Sum lst -> (
      match lst with
      | [] -> Const 0
      | hd :: tl -> Sum [ diff (hd, x); diff (Sum tl, x) ] )
