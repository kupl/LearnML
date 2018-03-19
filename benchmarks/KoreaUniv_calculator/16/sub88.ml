
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
 
  let rec calSigma
  = fun (a, b, ope, sum) -> if a=b then sum+ ope a
                            else calSigma(a+1, b, ope, sum+(ope a))

  let rec makeOpe : exp -> (int -> int)
  = fun e ->
    match e with
      | X -> (fun x -> x)
      | INT n -> (fun x -> n)
      | ADD (e1,e2) -> (fun x -> (((makeOpe e1) x) + ((makeOpe e2) x)))
      | SUB (e1,e2) -> (fun x -> (((makeOpe e1) x) - ((makeOpe e2) x)))
      | MUL (e1,e2) -> (fun x -> (((makeOpe e1) x) * ((makeOpe e2) x)))
      | DIV (e1,e2) -> (fun x -> (((makeOpe e1) x) / ((makeOpe e2) x)))
      | _ -> (fun x -> 0);;

  let rec calculator : exp -> int
  = fun exp ->
    match exp with
      | X -> 0
      | INT n -> n
      | ADD(e1, e2) -> calculator(e1)+calculator(e2)
      | SUB(e1, e2) -> calculator(e1)-calculator(e2)
      | MUL(e1, e2) -> calculator(e1)*calculator(e2)
      | DIV(e1, e2) -> calculator(e1)/calculator(e2)
      | SIGMA(e1, e2, ope) -> calSigma(calculator(e1), calculator(e2), makeOpe ope, 0) 
