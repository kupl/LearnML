type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp =
 fun (e, x) ->
  match e with
  | Const a -> Const 0
  | Var a -> if a = x then Const 1 else Const 0
  | Power (base, exp) ->
      if base = x then Times [ Const exp; Power (base, exp - 1) ] else Const 0
  | Times l -> (
      match l with
      | [] -> Const 0
      | [ _ ] -> Const 0
      | th :: tm :: tl ->
          Sum
            [
              Times [ diff (th, x); tm ];
              Times [ diff (tm, x); th ];
              diff (Times tl, x);
            ] )
  | Sum l -> (
      match l with
      | [] -> Const 0
      | [ _ ] -> Const 0
      | th :: tm :: tl -> Sum [ diff (th, x); diff (Sum tl, x) ] )
