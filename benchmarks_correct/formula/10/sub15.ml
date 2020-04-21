type formula = 
True
| False
| Not of formula
| AndAlso of formula * formula
| OrElse of formula * formula
| Imply of formula * formula
| Equal of exp * exp
and exp = 
Num of int
| Plus of exp * exp
| Minus of exp * exp

let rec eval f =
	let rec solve e =
		match e with
			Num a -> a
			| Plus (e1, e2) -> ((solve e1)+(solve e2))
			| Minus (e1, e2) -> ((solve e1)-(solve e2))
	in
	match f with
		True -> true
		| False -> false
		| Not f2 -> (not (eval f2))
		| AndAlso (f2, f3) -> ((eval f2)&&(eval f3))
		| OrElse (f2, f3) -> ((eval f2)||(eval f3))
		| Imply (f2, f3) -> ((not (eval f2)) || (eval f3))
		| Equal (e1, e2) -> ((solve e1) = (solve e2));;

(*
let testform = Imply (Equal (Num 45, Plus (Num 23, Minus (Num 65, Num 32))), AndAlso (OrElse (True, False),Not True));;
let testform2 = Imply (Equal (Num 45, Plus (Num 23, Minus (Num 65, Num 32))), AndAlso (OrElse (True, False),Not False));;
let testform3 = Imply (Equal (Num 145, Plus (Num 23, Minus (Num 65, Num 32))), AndAlso (OrElse (True, False),Not True));;
let testform4 = Imply (Equal (Num 145, Plus (Num 23, Minus (Num 65, Num 32))), AndAlso (OrElse (True, False),Not False));;
*)
