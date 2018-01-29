
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec sigma s f func e x
 = if f <= s then
   (func e s)
   else
   (func e f) + (sigma s (f-1) func e x)
      
let rec solver e m =
    match e with
    |X -> m
    |INT n -> n
    |ADD(e1,e2)-> (solver e1 m) + (solver e2 m)
    |SUB(e1,e2)-> (solver e1 m) - (solver e2 m)
    |MUL(e1,e2)-> (solver e1 m) * (solver e2 m)
    |DIV(e1,e2)-> (solver e1 m) / (solver e2 m)
    |SIGMA(e1,e2,e3)-> 
     sigma (solver e1 0) (solver e2 0) solver e3 (solver e1 0)

 (* let calculator : exp -> int
  = fun exp -> raise NotImplemented   TODO *)
  
let calculator : exp -> int
  = fun exp -> (solver exp 0)