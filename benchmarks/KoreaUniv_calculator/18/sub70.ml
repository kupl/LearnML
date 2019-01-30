type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp


let rec calculator : exp -> int
= fun exp -> match exp with
  | X -> 0
  | INT n -> n
  | ADD (a,b) -> calculator a + calculator b
  | SUB (a,b) -> calculator a - calculator b
  | MUL (a,b) -> calculator a * calculator b
  | DIV (a,b) -> calculator a / calculator b
  | SIGMA (k,n,f) ->  if(calculator k<= calculator n) then
                        calculator (ADD (f, (SIGMA (ADD(k,INT 1),n,f))))
                      else 0;;