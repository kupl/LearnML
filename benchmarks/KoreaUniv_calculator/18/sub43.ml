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
  let rec eval : exp*int -> int
    = fun (expr, x) ->
      match expr with
        | X -> x
        | INT a -> a
        | ADD(a,b) -> (eval (a,x))+(eval (b,x))
        | SUB(a,b) -> (eval (a,x))-(eval (b,x))
        | MUL(a,b) -> (eval (a,x))*(eval (b,x))
        | DIV(a,b) -> (eval (a,x))/(eval (b,x))
        | SIGMA(a,b,c) -> if((calculator a) = (calculator b)) then eval (c,(eval (b,x))) else (eval (c,(eval (a,x)))) + (eval (SIGMA (ADD(a,INT 1), b, c), x))
  in match exp with
    | X -> 0
    | INT a -> a
    | ADD(a,b) -> (calculator a)+(calculator b)
    | SUB(a,b) -> (calculator a)-(calculator b)
    | MUL(a,b) -> (calculator a)*(calculator b)
    | DIV(a,b) -> (calculator a)/(calculator b)
    | SIGMA(a,b,c) -> if((calculator a) = (calculator b)) then eval (c, (calculator b)) else eval (c, (calculator a)) + (calculator (SIGMA(ADD(a,INT 1), b, c)));;