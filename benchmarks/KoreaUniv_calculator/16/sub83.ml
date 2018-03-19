
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  
  let rec calX
  = fun i exp ->
  match exp with
  X -> i
 |INT(a) -> a
 |ADD(a,b) -> (calX i a) + (calX i b)
 |MUL(a,b) -> (calX i a) * (calX i b)
 |SUB(a,b) -> (calX i a) - (calX i b)
 |DIV(a,b) -> if((calX i b)=0) then raise NotImplemented else (calX i a)/(calX i b) 
 |SIGMA(a,b,c) -> if((calX i a)<=(calX i b)) then (calX i c) + (calX i (SIGMA(INT((calX i a)+1),b,c))) else 0 
 
 let rec calculator : exp -> int
  = fun exp ->
  match exp with
  X -> raise NotImplemented
 |INT(a) -> a
 |ADD(a,b) -> (calculator a)+(calculator b)
 |SUB(a,b) -> (calculator a)-(calculator b)
 |MUL(a,b) -> (calculator a)*(calculator b)
 |DIV(a,b) -> if((calculator b)=0) then raise NotImplemented else (calculator a)/(calculator b)	      
 |SIGMA(a,b,c) -> if((calculator a)<=(calculator b)) then (calX (calculator a) c) + calculator(SIGMA(INT((calculator a)+1),b,c)) else 0
