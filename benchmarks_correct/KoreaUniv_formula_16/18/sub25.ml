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

let rec fnum : exp -> int
= fun e -> match e with
  | Num x -> x
  | Plus(x,y) -> (fnum x) + (fnum y)
  | Minus(x,y) -> (fnum x) - (fnum y);;

let eval : formula -> bool
= fun f -> let rec findval : formula -> bool
= fun fm -> match fm with
  | True -> true
  | False -> false
  | Not x -> if findval x = true then false else true
  | AndAlso(x,y) -> if findval x = true && findval y = true then true else false
  | OrElse(x,y) -> if findval x = false && findval y = false then false else true
  | Imply(x,y) -> if findval x = true && findval y = false then false else true
  | Equal(x,y) -> if (fnum x) = (fnum y) then true else false
  in findval f;;
  
  eval (Imply (Imply (True, False), True));;
  eval (Equal (Num 1, Plus(Num 1, Num 2)));;