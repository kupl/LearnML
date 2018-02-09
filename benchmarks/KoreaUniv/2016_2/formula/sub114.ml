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

let rec equal_help exp=
match exp with
|Num n -> n
|Plus (exp1,exp2) -> (equal_help exp1) + (equal_help exp2)
|Minus (exp1,exp2) -> (equal_help exp1) - (equal_help exp2)

let rec eval f =
match f with
|True -> true
|False -> false
|Not f1 -> if eval f1 == true then false
           else true
|AndAlso (f1, f2) -> if (eval f1)&&(eval f2)== true then true
                     else false
|OrElse (f1,f2) -> if (eval f1)||(eval f2) == true then true
                   else false
|Imply (f1,f2) -> if ((eval f1)==true)&&(eval f2==false) then false
                  else true
|Equal (exp1,exp2)-> if (equal_help exp1)==(equal_help exp2) then true
                     else false
