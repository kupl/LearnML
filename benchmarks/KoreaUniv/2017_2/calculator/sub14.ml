(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e -> (* TODO *)
  let rec helper e = 
    match e with
    | INT i -> i
    | ADD (e1, e2) -> helper e1 + helper e2
    | SUB (e1, e2) -> helper e1 - helper e2
    | MUL (e1, e2) -> helper e1 * helper e2
    | DIV (e1, e2) -> helper e1 / helper e2 (*error when division by zero*)
    | SIGMA (e1, e2, e3) -> if helper e1 = helper e2 then (let X = e1 in helper e3) else (let X = e1 in helper e3) + helper (SIGMA(ADD(e1, INT 1), e2, e3)) in
      helper e
