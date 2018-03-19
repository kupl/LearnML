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

  let rec sigma : int -> int -> exp -> int
    =  fun s e f ->

    let rec calc : exp -> int -> int 
    =fun exp i ->
    match exp with
    | X -> i
    |INT n -> n
    |ADD (e1, e2) -> (calc e1 i) + (calc e2 i)
    |SUB (e1, e2) -> (calc e1 i) - (calc e2 i)
    |MUL (e1, e2) -> (calc e1 i) * (calc e2 i)
    |DIV (e1, e2) -> (calc e1 i) / (calc e2 i)
    |SIGMA (e1, e2, e3) -> (sigma (calc e1 i) (calc e2 i) e3) in
    if (s<=e) then ( sigma (s+1) e f )+( calc f s)
    else  0
    
  let rec calculator : exp -> int
  = fun exp ->
  match exp with
  |INT n -> n
  |ADD (e1, e2) -> (calculator (e1)) + (calculator (e2))
  |SUB (e1, e2) -> (calculator (e1)) - (calculator (e2))
  |MUL (e1, e2) -> (calculator (e1)) * (calculator (e2))
  |DIV (e1, e2) -> (calculator (e1)) / (calculator (e2))
  |SIGMA (e1, e2, e3) ->  sigma (calculator (e1)) (calculator (e2)) e3
  |_ -> raise NotImplemented
