type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp;;


let rec calculator : exp -> int
= fun exp -> (*TODO*)
  match exp with
    INT n -> n
    |ADD (m,n) -> calculator m+calculator n
    |SUB (m,n) -> calculator m-calculator n
    |MUL (m,n) -> calculator m*calculator n
    |DIV (m,n) -> calculator m/calculator n
    |SIGMA (m,n,k) -> 
      if calculator m = calculator n then 
      begin
      let rec cal exp =
      match exp with
        X -> calculator m
        |INT b -> b 
        |ADD (a,b) -> cal a+cal b
        |SUB (a,b) -> cal a-cal b
        |MUL (a,b) -> cal a*cal b
        |DIV (a,b) -> cal a/cal b
        |SIGMA (a,b,c) -> calculator (SIGMA(INT (cal a),INT (cal b),c))
      in cal k
      end
      else calculator (ADD(SIGMA(m,m,k),SIGMA(ADD(m,INT 1),n,k)));;
    
      calculator (SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1)));;
      
      
      
      
      
      
      
  