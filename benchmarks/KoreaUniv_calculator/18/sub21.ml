type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun exp ->
  match exp with
    | X -> raise (Failure "Wrong input.")
    | INT n -> n
    | ADD (a, b) -> (calculator a) + (calculator b)
    | SUB (a, b) -> (calculator a) - (calculator b)
    | MUL (a, b) -> (calculator a) * (calculator b)
    | DIV (a, b) -> (calculator a) / (calculator b)
    | SIGMA (a, b, e) -> (match a, b with
                            | INT n1, INT n2 -> if n1 <= n2 then (eval e n1) + (calculator (SIGMA (INT (n1 + 1), b, e))) else 0
                            | _, _ -> if (calculator a) <= (calculator b) 
                                      then (eval e (calculator a)) + calculator (SIGMA (INT ((calculator a) + 1), INT (calculator b), e)) else 0)
and eval : exp -> int -> int 
= fun exp nx ->
  match exp with
    | X -> nx
    | INT n -> n
    | ADD (a, b) -> (eval a nx) + (eval b nx)
    | SUB (a, b) -> (eval a nx) - (eval b nx)
    | MUL (a, b) -> (eval a nx) * (eval b nx)
    | DIV (a, b) -> (eval a nx) / (eval b nx)
    | SIGMA (a, b, e) -> calculator (SIGMA (INT (eval a nx), INT (eval b nx), e));;
