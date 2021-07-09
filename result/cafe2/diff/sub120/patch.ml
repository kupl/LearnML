type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((e : aexp), (x : string)) : aexp =
  match e with
  | Const n -> Const 0
  | Var str -> if str = x then Const 1 else Const 0
  | Power (str, n) ->
      if str != x then Const 0
      else if n = 1 then Const 1
      else Times [ Const n; Power (str, n - 1) ]
  | Times lst -> (
      match lst with
      | [] -> Const 0
      | hd :: t1 -> (
          match t1 with
          | [] ->
              Sum
                [
                  Times [ diff (hd, x); Const 1 ];
                  Times [ hd; diff (Times t1, x) ];
                ]
          | _ ->
              Sum
                [
                  Times [ diff (hd, x); Times t1 ];
                  Times [ hd; diff (Times t1, x) ];
                ] ) )
  | Sum lst -> (
      match lst with
      | [] -> Const 0
      | hd :: t1 -> Sum [ diff (hd, x); diff (Sum t1, x) ] )
