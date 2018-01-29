(* problem 5*)
type exp = X | INT of int | ADD of exp * exp | SUB of exp * exp | MUL of exp * exp | DIV of exp * exp | SIGMA of exp * exp * exp
let rec sigma_help : exp*int -> exp = fun (e,i) ->
   match e with
     X -> INT i
    | INT n -> INT n
    | ADD(e1, e2) -> ADD(sigma_help(e1,i), sigma_help(e2,i))
    | SUB(e1, e2) -> SUB(sigma_help(e1,i), sigma_help(e2,i))
    | MUL(e1, e2) -> MUL(sigma_help(e1,i), sigma_help(e2,i))
    | DIV(e1, e2) -> DIV(sigma_help(e1,i), sigma_help(e2,i))
    | SIGMA(e1, e2, e3) -> sigma_help(e3, i);;

let rec calculator : exp -> int = fun e ->
   match e with 
    | INT i -> i
    | ADD (e1, e2) -> calculator(e1) + calculator(e2)
    | SUB (e1, e2) -> calculator(e1) - calculator(e2)
    | MUL (e1, e2) -> calculator(e1) * calculator(e2)
    | DIV (e1, e2) -> calculator(e1) / calculator(e2)
    | SIGMA (e1, e2, e3) -> if calculator(e1) > calculator(e2) then 0 else  calculator(sigma_help(SIGMA(e1, e2, e3),calculator(e1))) + calculator(SIGMA(ADD(e1, INT 1),e2,e3));;
