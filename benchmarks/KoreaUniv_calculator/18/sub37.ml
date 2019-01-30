type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  
exception Where_is_const;;

let rec calculator : exp -> int
= fun exp -> 
  match exp with
  | X -> raise Where_is_const
  | INT n -> n
  | ADD(a,b) -> (calculator a) + (calculator b)
  | SUB(a,b) -> (calculator a) - (calculator b)
  | MUL(a,b) -> (calculator a) * (calculator b)
  | DIV(a,b) -> (calculator a) / (calculator b)
  | SIGMA( f,  l, e)->  
    let rec sigma s d ep =
      if s > d then 0 else (cal ep s) + (sigma (s+1) d ep )
    in sigma (calculator f) (calculator l) e  
    
and
  cal ex x =
  match ex with
  | X -> x
  | INT n -> n
  | ADD(a,b) -> (cal a x) + (cal b x)
  | SUB(a,b) -> (cal a x) - (cal b x)
  | MUL(a,b) -> (cal a x) * (cal b x)
  | DIV(a,b) -> (cal a x) / (cal b x)
  | SIGMA( f,  l, e)->  
    let rec sigma s d ep =
      if s > d then 0 else (cal ep s) + (sigma (s+1) d ep )
    in sigma (calculator f) (calculator l) e  
;;

