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
	| Minus of exp * exp ;;

let rec cal x =
match x with
|Num n -> n
|Plus (n1, n2) -> (cal n1) + (cal n2)
|Minus (n1, n2) -> (cal n1) - (cal n2);;
	
	
let rec eval : formula -> bool
= fun f -> match f with
|True -> true
|False -> false
|Not (n) -> if (eval n) then false else true
|AndAlso (n1, n2) -> (eval n1) && (eval n2)
|OrElse (n1, n2) -> (eval n1) || (eval n2)
|Imply (n1, n2) -> if((eval n1) && not(eval n2)) then false else true
|Equal (n1, n2) -> 
	let r1 = cal n1 in
	let r2 = cal n2 in
	if(r1 == r2) then true else false;;

