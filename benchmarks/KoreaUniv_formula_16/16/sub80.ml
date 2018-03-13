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

let rec eee : exp -> int
= fun e -> match e with | Num b -> b | Plus(b,c) ->(eee b)+(eee c)
												| Minus(b,c) -> (eee b)-(eee c);;

let rec eval : formula -> bool
= fun f -> match f with |True ->true |False ->false|Not a ->not (eval a)
												|AndAlso (a,b) -> (eval a)&& (eval b)
												|OrElse (a,b) -> (eval a)|| (eval b)
												|Imply(a,b) -> if ((eval a) = true)&&((eval b) = false) then false else true
												|Equal(a,b) -> (eee a) = (eee b) ;;
