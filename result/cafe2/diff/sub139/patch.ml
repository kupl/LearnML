type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((e : aexp), (x : string)) : aexp =
  match e with
  | Const b -> Const 0
  | Var a -> if a != x then Const 0 else Const 1
  | Power (a, b) ->
      if a != x then Const 0 else Times [ Const b; Power (a, b - 1) ]
  | Times l -> (
      match l with
      | [] -> Const 0
      | head :: tail ->
          Sum
            [
              Times ([ diff (head, x) ] @ tail);
              Times [ head; diff (Times tail, x) ];
            ] )
  | Sum l2 -> (
      match l2 with
      | [] -> Sum l2
      | head :: tail -> Sum [ diff (head, x); diff (Sum tail, x) ] )
