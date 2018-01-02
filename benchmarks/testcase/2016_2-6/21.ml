type exp = 
|Num of int 
|Plus of exp * exp
|Minus of exp * exp

type formula = 
|True 
|False
|Not of formula
|AndAlso of formula * formula
|OrElse of formula * formula
|Imply of formula * formula
|Equal of exp * exp 


let rec evalexp f =
match f with
|Num x -> x
|Plus (x,y)->(evalexp x)+(evalexp y)
|Minus (x,y)->(evalexp x)+(evalexp y)
;;
let rec f form = 
	match form with 
	| True -> true
	| False -> false 
	| Not (p) -> if (f p) = true then false
				else true
	|AndAlso(p,q)-> if (f p)=true &&(f q) = true then true  
	                  else false
	|OrElse(p,q)->   if (f p)=true ||(f q) = true then true  
	                  else false 
	|Imply(p,q)-> if (f q)=true then true 
				 else false
	|Equal(p,q)-> if(evalexp p)=(evalexp q) then true 
				else false                             
;;   