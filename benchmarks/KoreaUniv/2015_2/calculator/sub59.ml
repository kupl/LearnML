type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let calculator : exp -> int
=fun e -> 
  let rec eval exp = X in
  match e with
    X -> X 
  | INT v -> v
  | ADD (e,e') -> calculator e + calculator e'
  | SUB (e,e') -> calculator e - calculator e'
  | MUL (e,e') -> calculator e * calculator e'
  | DIV (e,e') -> calculator e / calculator e'
  | SIGMA (e,e',e'') -> 
        if e = e' then calculator e''
      else calculator e'' + calculator (SIGMA(ADD(e , INT 1), e', e''));;
