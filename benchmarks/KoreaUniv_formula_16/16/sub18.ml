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
= fun f -> match f with
	| True -> true
	| False -> false
	| Not (for_val) -> not (eval for_val)
	| AndAlso (left, right) -> eval left && eval right
	| OrElse (left, right) -> eval left || eval right
	| Imply (left, right) -> if eval left = true then eval right == true else true
	| Equal (left, right) -> 
	let rec eval_exp : exp -> int = fun x -> match x with
	| Num (num_value) -> num_value
	| Plus (left, right) -> eval_exp left + eval_exp right
	| Minus (left, right) -> eval_exp left - eval_exp right
	in eval_exp left == eval_exp right (* TODO *)
;;
