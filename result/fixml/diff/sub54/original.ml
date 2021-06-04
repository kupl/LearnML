type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec map f l = match l with [] -> [] | hd :: tl -> f hd :: map f tl

let rec diff : aexp * string -> aexp =
 fun (aexp, x) ->
  match aexp with
  | Sum l ->
      Sum
        ( match l with
        | [] -> [ Const 0 ]
        | hd :: tl -> [ diff (hd, x); diff (Sum tl, x) ] )
  | Const n -> Const 0
  | Var x -> Const 1
  | Power (x, n) -> (
      match n with 1 -> Const 1 | n -> Times [ Const n; Power (x, n - 1) ] )
  | Times l -> (
      match l with
      | [] -> Const 0
      | [ Const a; Const b; ax ] -> Times [ Const (a * b); diff (ax, x) ]
      | [ Const a; ax ] -> Times [ Const a; diff (ax, x) ]
      | [ Const a ] -> Const 0
      | [ ax ] -> diff (ax, x) )
