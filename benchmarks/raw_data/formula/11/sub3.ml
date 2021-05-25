(* 200511843 LEE JONGHO *)

type formula = True
	| False
	| Not of formula
	| AndAlso of formula * formula
	| OrElse of formula * formula
	| Imply of formula * formula
	| Equal of exp * exp
and exp = Num of int
	| Plus of exp * exp
	| Minus of exp * exp


let imply (f1, f2) =
	if f1 = true then if f2 = true then true else false
	             else true

let orelse (f1, f2) =
	if f1 = true || f2 = true then true
				  else false

let andalso (f1, f2) =
	if f1 = true && f2 = true then true
				  else false


let less (e1, e2) =
	let rec cal ex =
		match ex with	
		Num x -> x
		| Plus (e1, e2) -> (cal e1)+(cal e2)
		| Minus (e1, e2) -> (cal e1)-(cal e2) in
	if (cal e1) = (cal e2) then true
		               else false

 

let rec eval f =
	match f with
	True -> true
	| False -> false
	| Not fom -> not (eval fom)
	| Imply (f1, f2) -> imply ((eval f1), (eval f2))
	| Equal (e1, e2) -> less (e1, e2)
	| OrElse (f1, f2) -> orelse ((eval f1), (eval f2))
	| AndAlso (f1, f2) -> andalso ((eval f1), (eval f2))
