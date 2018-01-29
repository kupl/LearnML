
exception IllegalInput

  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

   let rec sigma2 : exp * int -> int
   = fun (exp,n) ->
   match exp with
   | X -> n
   | INT (n) -> n
   | ADD(e1,e2) -> sigma2(e1,n)+sigma2(e2,n)
   | SUB(e1,e2) -> sigma2(e1,n)-sigma2(e2,n)
   | MUL(e1,e2) -> sigma2(e1,n)*sigma2(e2,n)
   | DIV(e1,e2) -> sigma2(e1,n)/sigma2(e2,n)
   | SIGMA(e1,e2,e3) ->
      if sigma2(e1, n) > sigma2(e2, n) then raise IllegalInput
      else if sigma2(e1, n) = sigma2(e2, n) then sigma2(e3, sigma2(e1, n))
      else sigma2(e3, sigma2(e1, n)) + sigma2(SIGMA(INT (sigma2(e1, n) +1), INT (sigma2(e2, n)), e3),n)

   and calculator : exp -> int
  = fun exp -> (* TODO *)
   match exp with
   | X -> raise IllegalInput
   | INT(n) -> n
   | ADD(e1,e2) -> calculator(e1)+calculator(e2)
   | SUB(e1,e2) -> calculator(e1)-calculator(e2)
   | MUL(e1,e2) -> calculator(e1)*calculator(e2)
   | DIV(e1,e2) -> calculator(e1)/calculator(e2)
   | SIGMA(e1,e2,e3) ->
      if calculator(e1) > calculator(e2) then raise IllegalInput
      else if calculator(e1) = calculator(e2) then sigma2(e3, calculator(e1))
      else sigma2(e3, calculator(e1)) + calculator(SIGMA(INT (calculator(e1) +1), INT (calculator(e2)), e3))
