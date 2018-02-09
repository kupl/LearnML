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

let rec expl e =
	match e with
	| Num n -> n
	| Plus (e1,e2) -> expl e1 + expl e2
	| Minus (e1,e2) -> expl e1 - expl e2;;

let rec eval f=
	match f with
	| True -> True
	| False ->False
	| Not f1 -> if(f1= True) then False else True
	| AndAlso (f1, f2) -> if(f1=True && f2 =True) then True else False
	| OrElse (f1, f2) -> if(f1=False && f2= False)then False else True
	| Imply (f1, f2) -> if(f2 = False && f1 = True) then False else True
	| Equal  (e1, e2) -> if(expl e1==expl e2) then True else False;;		
