type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec cal : (int -> int -> int) -> exp -> exp -> int
=fun op e1 e2 ->
let v1 = calculator e1 in
let v2 = calculator e1 in
(match v1, v2 with
| INT n1, INT n2 -> INT (op n1 n2)
| _ -> raise (Failure "ERROR: X is not defined"));;


let calculator : exp -> int
=fun e -> match e with
| INT a -> a
| ADD(e1,e2) -> cal (+) e1 e2
| SUB(e1,e2) -> cal (-) e1 e2
| MUL(e1,e2) -> cal (*) e1 e2
| DIV(e1,e2) -> cal (/) e1 e2