type exp = 
	| Num of int 
	| Plus of exp * exp 
	| Minus of exp * exp 

type formula =
	| True
	| False 
	| Not of formula 
	| AndAlso of formula * formula 
	| OrElse of formula * formula 
	| Imply of formula * formula 
	| Equal of exp * exp

let rec f : formula -> bool
= fun form -> match form with 
 | True -> true
 | False -> false
 | Not (a) -> if f a = true then true else false
 | AndAlso(a,b) -> if f(a)=true && f(b) = true then true else false
 | OrElse (a,b)-> if f(a)=false && f(b) = false then false else true
 | Imply (a,b) -> if f(a)=true && f(b) = false then false else true
 | Equal (a,b) -> let rec eq : exp-> int =  fun e ->
 match e with 
 | Num (a)->a
 | Plus(a,b)-> eq(a)+eq(b)
 | Minus (a,b)-> eq(a) - eq(b) in
 if(eq)a = eq (b) then true else false


