
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec sigma : exp -> int -> int
  = fun exp var -> match exp with
  | X -> var
  | INT n -> n
  | ADD (n1, n2) -> sigma n1 var + sigma n2 var
  | SUB (n1, n2) -> sigma n1 var - sigma n2 var
  | MUL (n1, n2) -> sigma n1 var * sigma n2 var
  | DIV (n1, n2) -> sigma n1 var / sigma n2 var
  | SIGMA (n1, n2, n3) -> if sigma n1 var = sigma n2 var then sigma n3 (sigma n1 var) else
   sigma n3 (sigma n1 var) + sigma (SIGMA (INT (sigma n1 var + 1), n2, n3)) var

  let rec calculator : exp -> int
  = fun exp -> match exp with
  | INT n -> n
  | ADD (n1, n2) -> calculator n1 + calculator n2
  | SUB (n1, n2) -> calculator n1 - calculator n2
  | MUL (n1, n2) -> calculator n1 * calculator n2
  | DIV (n1, n2) -> calculator n1 / calculator n2
  | SIGMA (n1, n2, n3) -> if calculator n1 = calculator n2 then sigma n3 (calculator n1)
   else sigma n3 (calculator n1) + calculator (SIGMA (INT (calculator n1 + 1), n2, n3))