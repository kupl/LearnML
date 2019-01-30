type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp

let rec calculator : exp -> int
= fun exp -> match exp with
  | INT x -> x
  | ADD (a, b) -> calculator a + calculator b
  | SUB (a, b) -> calculator a - calculator b
  | MUL (a, b) -> calculator a * calculator b
  | DIV (a, b) -> calculator a / calculator b
  (*init by a and bound by c*)
  ;;
  
let a = 5;;
let b = 3;;
calculator(SUB(MUL(INT a, INT b), INT 1));;