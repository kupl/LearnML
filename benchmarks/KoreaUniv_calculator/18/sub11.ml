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
  | X -> raise(Failure"Put the value into X")
  | INT a -> a
  | ADD (a,b) -> calculator a  + calculator b 
  | SUB (a,b) -> calculator a  - calculator b 
  | MUL (a,b) -> calculator a  * calculator b 
  | DIV (a,b) -> calculator a  / calculator b 
  | SIGMA (a, b, f) -> let rec inf g = match g with
    | X -> inf a
    | INT a' -> a'
    | ADD (a', b') -> (inf a') + (inf b')
    | SUB (a', b') -> (inf a') - (inf b')
    | MUL (a', b') -> (inf a') * (inf b')
    | DIV (a', b') -> (inf a') / (inf b')
    | SIGMA (a', b', f') -> calculator (SIGMA(a', b', f'))
      in if calculator a <= calculator b then inf f + calculator(SIGMA(ADD(a, INT 1), b, f)) else 0;;
      