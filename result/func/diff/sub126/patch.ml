exception Problem

type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((e : aexp), (x : string)) : aexp =
  match e with
  | Const _ -> Const 0
  | Var y -> if x = y then Const 1 else Const 0
  | Power (y, n) ->
      if x = y then Times [ Const n; Power (y, n - 1) ] else Const 0
  | Times l -> (
      match l with
      | [] -> raise Problem
      | [ a1; a2 ] ->
          Sum [ Times [ diff (a1, x); a2 ]; Times [ a1; diff (a2, x) ] ]
      | [ __s63 ] -> diff (__s63, x)
      | hd :: tl ->
          Sum
            [
              Times [ diff (hd, x); Times tl ]; Times [ hd; diff (Times tl, x) ];
            ] )
  | Sum l -> (
      match l with
      | [] -> Const 0
      | [ a1; a2 ] -> Sum [ diff (a1, x); diff (a2, x) ]
      | hd :: tl -> Sum [ diff (hd, x); diff (Sum tl, x) ] )
