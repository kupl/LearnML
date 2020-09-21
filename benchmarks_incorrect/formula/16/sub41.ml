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

let andAlso : bool * bool -> bool
= fun f -> match f with
  	| (true, true) -> true
  	| _ -> false

let orElse : bool * bool -> bool
= fun f -> match f with
	| (false, false) -> false
	| _ -> true

let imply : bool * bool -> bool
= fun f -> match f with
	| (frue, false) -> false
	| _ -> true

let rec ca : exp -> int
= fun f -> match f with
	| Num a -> a
	| Plus (a, b) -> (ca a) + (ca b)
	| Minus (a, b) -> (ca a) - (ca b)


let equal : exp * exp -> bool
= fun f -> match f with
	|(a,b) -> if ca a = ca b then true else false





let rec eval : formula -> bool
= fun f -> match f with
	| True -> true
	| False -> false
	| AndAlso (a,b) -> andAlso (eval a, eval b)
	| OrElse (a,b) -> orElse (eval a, eval b)
	| Imply (a,b) -> imply (eval a, eval b)
	| Equal (a,b) -> equal (a, b)
	| _ -> raise (Failure "eval requires formula") 

