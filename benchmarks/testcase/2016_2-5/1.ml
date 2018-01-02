type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
 match n1 with
  |ZERO -> n2
  |SUCC(nest) -> SUCC(natadd nest n2);; 

let rec f : nat -> nat -> nat
= fun n1 n2 -> 
 match n1, n2 with 
 |_,ZERO -> SUCC(ZERO)
 |ZERO,SUCC(nest2) -> SUCC(f n1 nest2)
 |SUCC(nest1),_ -> SUCC(f nest1 n2);;

(*********************)
(*     Problem 6     *)
(*********************)
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

let rec match_exp : exp -> int
 = fun e ->
	match e with
	|Num e1 -> e1
	|Plus (e1, e2) -> match_exp e1 + match_exp e2
	|Minus (e1, e2) -> match_exp e1 - match_exp e2;;

let rec eval : formula -> bool
= fun f -> 
  match f with
  |True -> true
  |False -> false 
  |Not n -> not(eval n)
  |AndAlso (fo1,fo2) -> (eval fo1) && (eval fo2)
  |OrElse (fo1,fo2) -> (eval fo1) || (eval fo2)
  |Imply (fo1,fo2) -> not (eval fo1) || (eval fo2)  
  |Equal (e1,e2) -> 
	let v1 = match_exp e1 in
	let v2 = match_exp e2 in
	if v1 = v2 then true else false;;



