
  type exp = X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp


let rec calculator : exp -> int
= fun a -> match a with
  | X -> raise (Failure "X is ambiguous")
  | INT n -> n
  | ADD (n1,n2) -> calculator n1 + calculator n2
  | SUB (n1,n2) -> calculator n1 - calculator n2
  | MUL (n1,n2) -> calculator n1 * calculator n2
  | DIV (n1,n2) -> calculator n1 / calculator n2
  | SIGMA (first ,second ,exp3) ->
    let one = calculator first and two = calculator second in
      if one > two then 0 else sigma (exp3,first) + calculator( SIGMA (ADD (first,INT 1),second, exp3))
    and sigma (equ,nn) =
    match equ with
    |X -> calculator nn
    |INT n -> n
    |ADD (n1,n2) -> sigma(n1,nn) + sigma (n2,nn)
    |SUB (n1,n2) -> sigma(n1,nn) - sigma (n2,nn)
    |MUL (n1,n2) -> sigma(n1,nn) * sigma (n2,nn)
    |DIV (n1,n2) -> sigma(n1,nn) / sigma (n2,nn)
    |SIGMA (n1,n2,eqq) -> calculator (SIGMA (INT(sigma(n1,nn)),INT(sigma(n2,nn)),eqq))