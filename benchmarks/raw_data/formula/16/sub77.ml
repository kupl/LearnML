type formula =
    | True
    | False
    | Not of formula
    | AndAlso of formula * formula
    | OrElse of formula * formula
    | Imply of formula * formula
    | Equal of exp * exp
  
  and exp =
    | Num of int
    | Plus of exp * exp
    | Minus of exp * exp
  
  let rec eval : formula -> bool
  = fun f -> 
    let rec evalExp : exp -> int = fun e
    -> match e with
    |Num(n) -> n
    |Plus(n1,n2) -> evalExp(n1)+evalExp(n2)
    |Minus(n1,n2) -> evalExp(n1)-evalExp(n2)
    in match f with
 |True -> true
 |False -> false
 |Not f1 -> if (eval f1) = true then false else true
 |AndAlso(f1,f2)  -> if (eval f1)=true && (eval f2)=true then true else false
 |OrElse(f1,f2) -> if (eval f1)=true || (eval f2)=true then true else false
 |Imply(f1,f2) -> if (eval f1)=true then eval f2 else true
 |Equal(e1,e2) -> if (evalExp e1) = (evalExp e2) then true else false 
