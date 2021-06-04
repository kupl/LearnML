type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp =
 fun (e, x) ->
  match e with
  | Const b -> Const 0
  | Var a -> if a = x then Const 1 else Var a
  | Power (a, b) ->
      if a = x then Times [ Const b; Power (a, b - 1) ] else Power (a, b)
  | Times l -> (
      match l with
      | [] -> Times l
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
