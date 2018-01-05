type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp;;
exception FreeVariable;;


let galculator: exp -> float = fun e ->
  let rec galdo : exp * float -> float = fun (ex,fb) ->
    match ex with
    | X -> match fb with
           | [] -> FreeVariable
           | hd::tl -> hd

    | INT(i)  -> (float_of_int i)
    | REAL(f) -> f 
    | ADD(ex1,ex2) -> (galdo (ex1,fb))+(galdo (ex2,fb))
    | SUB(ex1,ex2) -> (galdo (ex1,fb))-(galdo (ex2,fb))
    | MUL(ex1,ex2) -> (galdo (ex1,fb))*(galdo (ex2,fb))
    | DIV(ex1,ex2) -> (galdo (ex1,fb))/(galdo (ex2,fb))
    | SIGMA(ex1,ex2,ex3) -> (galdo (ex3,0))
    | INTEGRAL(ex1,ex2,ex3) -> (galtail (ex,0))
  in
  (galdo e)
;;
