type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp =
 fun (aexp, x) ->
  match aexp with
  | Const n -> Const 0
  | Var x -> Const 1
  | Power (x, n) -> (
      match n with
      | 0 -> Const 1
      | _ -> Times [ Const n; diff (Power (x, n - 1), x) ] )
  | Times a :: b :: tl -> Times [ a; diff (b, x) ]
  | Sum hd :: tl -> (
      match hd with
      | Const 0 -> Const 0
      | _ -> Sum [ diff (hd, x); diff (Sum tl, x) ] )
  | Times [] -> raise Failure "error"
