
(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented

  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec exp_x : exp -> int -> int
  = fun exp value ->
    match exp with
    | INT n -> n
    | X -> value
    | ADD (e1, e2) -> (exp_x e1 value) + (exp_x e2 value)
    | SUB (e1, e2) -> (exp_x e1 value) - (exp_x e2 value)
    | MUL (e1, e2) -> (exp_x e1 value) * (exp_x e2 value)
    | DIV (e1, e2) -> (exp_x e1 value) / (exp_x e2 value);;

  let rec calculator : exp -> int
  = fun exp -> 
    match exp with
    | INT n -> n
    | ADD (INT n1, INT n2) -> n1 + n2
    | SUB (INT n1, INT n2) -> n1 - n2
    | MUL (INT n1, INT n2) -> n1 * n2
    | DIV (INT n1, INT n2) -> n1 / n2
    | SIGMA (start_e, finish_e, func) ->
        let start = calculator start_e in 
        let finish = calculator finish_e in
        if start > finish then 0
        else if start == finish then (exp_x func start)
        else (exp_x func start) + calculator (SIGMA (INT (start+1), INT finish, func))
    |_ -> raise NotImplemented;;
