type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  
let rec calculator : exp -> int
= fun e -> 
  match e with
    | X -> 0
    | INT a -> a
    | ADD (a,b) -> (calculator a) + (calculator b)
    | SUB (a,b) -> (calculator a) - (calculator b)
    | MUL (a,b) -> (calculator a) * (calculator b)
    | DIV (a,b) -> (calculator a) / (calculator b)
    | SIGMA (a,b,c) -> 
      if (calculator(b) > calculator(a)) then 0 else 
        if (calculator(a)=calculator(b)) then sigma c b else
          sigma c a + (calculator(SIGMA (INT (calculator(a)+1)),calculator(b),c));;
            
