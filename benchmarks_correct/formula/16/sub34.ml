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
 
match f with
|True -> true
|False -> false
|Not(k) -> if(eval(k)=eval(True)) then false else true
|AndAlso(p,q) -> if((eval(p)=eval(True))&&(eval(q)=eval(True))) then true else false
|OrElse(p,q) -> if((eval(p)=eval(False))&&(eval(q)=eval(False))) then false else true
|Imply(p,q)-> if(eval(p)=eval(True)&&eval(q)=eval(False)) then false else true
|Equal(p,q) -> if((eval2 p)=(eval2 q)) then true else false

and eval2
= fun g ->
begin
match g with
|Num(i) -> i
|Plus(j,k)->eval2(j)+eval2(k)
|Minus (j,k) -> eval2(j)-eval2(k)
end;;

  

 (* TODO *)

