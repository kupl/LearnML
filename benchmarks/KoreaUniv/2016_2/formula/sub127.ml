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
= fun f -> (* TODO *)
match f with
True -> true
|False -> false
|Not x -> not (eval x)
|AndAlso (x,y) -> (eval x) && (eval y)
|OrElse (x,y) -> (eval x) || (eval y)
|Imply (x,y) -> if ((eval x)=false) then true else (eval y)
|Equal (x,y) ->  
  let rec calcul = fun expr ->
    match expr with
     Num a -> a
    |Plus(m, n) -> (calcul m) + (calcul n)
    |Minus(m, n) -> (calcul m) - (calcul n)
  in
   if ((calcul x)=(calcul y)) then true else false
