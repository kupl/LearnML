type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun exp -> 
  match exp with
    |INT n -> n
    |ADD (a,b) -> calculator a + calculator b
    |SUB (a,b) -> calculator a - calculator b
    |MUL (a,b) -> calculator a * calculator b
    |DIV (a,b) -> calculator a / calculator b
    |SIGMA (a,b,c) -> 
      let rec sigcal: int->exp->int 
      = fun x c ->
        match c with
         |INT n -> n
         |ADD (c,d) -> if c==X then x + calculator d 
                        else if d==X then calculator c + x
                        else calculator c + calculator d
         |SUB (c,d) -> if c==X then x - calculator d 
                        else if d==X then calculator c - x
                        else calculator c - calculator d
         |MUL (c,d) -> if c==X then x * calculator d 
                        else if d==X then calculator c * x
                        else calculator c * calculator d
         |DIV (c,d) -> if c==X then x / calculator d 
                        else if d==X then calculator c / x
                        else calculator c / calculator d
      in if a==b then sigcal (calculator a) c else sigcal (calculator a) c + calculator (SIGMA(INT((calculator a) +1),b,c));;
      