type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  

let rec eval : exp -> int -> int
= fun e x -> 
  let rec sigma
  = fun e x n sum -> 
    if x > n then sum
    else sigma e (x+1) n sum+(eval e x) in
  match e with
  | X -> x
  | INT n -> n
  | ADD (e1, e2) -> eval e1 x + eval e2 x
  | SUB (e1, e2) -> eval e1 x - eval e2 x
  | MUL (e1, e2) -> eval e1 x * eval e2 x
  | DIV (e1, e2) -> eval e1 x / eval e2 x
  | SIGMA (e1, e2, e3) -> sigma e3 (eval e1 x) (eval e2 x) 0;;

let rec calculator : exp -> int
= fun exp -> 
  eval exp 0;;
  