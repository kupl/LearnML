(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun e -> match e with
  | X -> raise (Failure "not applicable")
  | SIGMA(e1, e2, e3) ->
    let rec evaluateExpInSig : exp -> int -> int
        = fun expWithX xValue ->
        match expWithX with
        | X -> xValue
        | INT (i) -> i
        | ADD (e1, e2) -> (evaluateExpInSig e1 xValue) + (evaluateExpInSig e2 xValue)
        | SUB (e1, e2) -> (evaluateExpInSig e1 xValue) - (evaluateExpInSig e2 xValue)
        | MUL (e1, e2) -> (evaluateExpInSig e1 xValue) * (evaluateExpInSig e2 xValue)
        | DIV (e1, e2) -> (evaluateExpInSig e1 xValue) / (evaluateExpInSig e2 xValue)
        | SIGMA (e'1, e'2, e'3) -> raise (Failure "not applicable with this function ability")
        
      in let rec summation : int -> int -> exp -> int
            = fun init bound expWithX ->
              if init > bound then 0
              else (evaluateExpInSig expWithX init) + (summation (init +1) bound expWithX)
          in (summation (evaluateExpInSig e1 0) (evaluateExpInSig e2 0) e3)

  | INT (i) -> i
  | ADD (e1, e2) -> calculator(e1) + calculator(e2)
  | SUB (e1, e2) -> calculator(e1) - calculator(e2)
  | MUL (e1, e2) -> calculator(e1) * calculator(e2)
  | DIV (e1, e2) -> calculator(e1) / calculator(e2)

