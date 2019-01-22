(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let diff : aexp * string -> aexp
= fun (e,x) -> match e with | Const c->Const 0 | Var c -> if c = x then Const 1 else Var c | Power (c,d) ->  if c = x then Times[Const d; Power(c,d-1)] else Power(c,d)
