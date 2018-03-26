type exp =
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp



let rec calculator : exp -> int    
=fun e -> match e with 
          | INT a -> a
          | ADD (a, b) -> calculator a + calculator b
          | SUB (a, b) -> calculator a - calculator b
          | MUL (a, b) -> calculator a * calculator b
          | DIV (a, b) -> calculator a / calculator b
