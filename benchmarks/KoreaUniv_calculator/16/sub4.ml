
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec calculator : exp -> int
  = fun exp -> 
      let rec substitute : exp -> int -> int
      = fun exp n ->
        match exp with
        | X -> n
        | INT i -> i
        | ADD(e1,e2) -> substitute e1 n + substitute e2 n
        | SUB(e1,e2) -> substitute e1 n - substitute e2 n
        | MUL(e1,e2) -> substitute e1 n * substitute e2 n
        | DIV(e1,e2) -> substitute e1 n / substitute e2 n
        | _ -> raise (Failure "can not substitute SIGMA")
    in match exp with
    | X -> raise (Failure "can not calculator")
    | INT i -> i
    | ADD(e1,e2) -> calculator e1 + calculator e2
    | SUB(e1,e2) -> calculator e1 - calculator e2
    | MUL(e1,e2) -> calculator e1 * calculator e2
    | DIV(e1,e2) -> calculator e1 / calculator e2
    | SIGMA(e1,e2,e3) -> 
      if calculator e1 > calculator e2 then 0
      else substitute e3 (calculator e1) + calculator(SIGMA(INT((calculator e1) + 1),e2,e3))