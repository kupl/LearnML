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
 
 let rec pog k
 =match k with
 |Num q->q
 |Plus (q,w) -> pog q + pog w
 |Minus (q,w) -> pog q - pog w
 
 let rec eval : formula -> bool
 = fun f-> match f with
 |True -> true
 |False -> false
 |Not  a -> if eval a = true then false else true
 |AndAlso  (a,b) -> eval a && eval b
 |OrElse  (a,b) -> eval a || eval b
 |Imply  (a,b) -> if eval a = true && eval b = false  then false else true
 |Equal  (x,y) ->  
	if (pog x) =(pog y) then true else false

