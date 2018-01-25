type exp =(* X*)
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
        (* | SIGMA of exp * exp * exp*)

(*let rec sigma : int * int * (int -> int) -> int
=fun (a, b, f) -> if a = b then f (a)
                  else ((f (b) + sigma (a, b-1, f))) *)


let rec calculator : exp -> int    
=fun e -> match e with 
           (*X -> calculator X*)
          | INT a -> a
          | ADD (a, b) -> calculator a + calculator b
          | SUB (a, b) -> calculator a - calculator b
          | MUL (a, b) -> calculator a * calculator b
          | DIV (a, b) -> calculator a / calculator b
        (*  | SIGMA (a, b, f) -> sigma (calculator a, calculator b, calculator f) *)

