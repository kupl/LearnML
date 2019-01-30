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
  let rec eval ex =
  match ex with
    | X -> fun x -> x
    | INT(i) -> fun x -> i
    | ADD(ex1, ex2) -> fun x -> ((eval ex1) x)+((eval ex2) x)
    | SUB(ex1, ex2) -> fun x -> ((eval ex1) x)-((eval ex2) x)
    | MUL(ex1, ex2) -> fun x -> ((eval ex1) x)*((eval ex2) x)
    | DIV(ex1, ex2) -> fun x -> ((eval ex1) x)/((eval ex2) x)
    | SIGMA(ex1, ex2, ex3) -> fun x -> calculator(SIGMA(ex1, ex2, ex3))
  in
  match exp with
    | X -> 0
    | INT(i) -> i
    | ADD(ex1, ex2) -> calculator(ex1) + calculator(ex2)
    | SUB(ex1, ex2) -> calculator(ex1) - calculator(ex2)
    | MUL(ex1, ex2) -> calculator(ex1) * calculator(ex2)
    | DIV(ex1, ex2) -> calculator(ex1) / calculator(ex2)
    | SIGMA(ex1, ex2, ex3) -> 
      let rec iter low high sum = 
        if low>high then sum
        else iter (low+1) high (sum + (eval ex3) low)
      in
      iter (calculator(ex1)) (calculator(ex2)) 0
;;