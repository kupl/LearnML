
(*problem 5 *)

type exp =
    X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  
let rec sub (f,x) = 

    match f with

    | X -> x
    | INT z -> z
    | ADD (a1,b2) -> sub (a1,x) + sub (b2,x)
    | SUB (a1,b2) -> sub (a1,x) - sub (b2,x)
    | MUL (a1,b2) -> sub (a1,x) * sub (b2,x)
    | DIV (a1,b2) -> sub (a1,x) / sub (b2,x)
    | SIGMA (a,b,f1) -> sub (SIGMA (ADD(a,INT(1)),b, f1),x)


let rec calculator e : exp->int
= fun e-> sub(e,1)
 