type formula = 
TRUE
| FALSE
| NOT of formula
| ANDALSO of formula * formula
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr
and expr = 
NUM of int
| PLUS of expr * expr
| MINUS of expr * expr

let rec eval f =
	let rec solve e =
		match e with
			NUM a -> a
			| PLUS (e1, e2) -> ((solve e1)+(solve e2))
			| MINUS (e1, e2) -> ((solve e1)-(solve e2))
	in
	match f with
		TRUE -> true
		| FALSE -> false
		| NOT f2 -> (not (eval f2))
		| ANDALSO (f2, f3) -> ((eval f2)&&(eval f3))
		| ORELSE (f2, f3) -> ((eval f2)||(eval f3))
		| IMPLY (f2, f3) -> ((not (eval f2)) || (eval f3))
		| LESS (e1, e2) -> ((solve e1) < (solve e2));;

(*
let testform = IMPLY (LESS (NUM 45, PLUS (NUM 23, MINUS (NUM 65, NUM 32))), ANDALSO (ORELSE (TRUE, FALSE),NOT TRUE));;
let testform2 = IMPLY (LESS (NUM 45, PLUS (NUM 23, MINUS (NUM 65, NUM 32))), ANDALSO (ORELSE (TRUE, FALSE),NOT FALSE));;
let testform3 = IMPLY (LESS (NUM 145, PLUS (NUM 23, MINUS (NUM 65, NUM 32))), ANDALSO (ORELSE (TRUE, FALSE),NOT TRUE));;
let testform4 = IMPLY (LESS (NUM 145, PLUS (NUM 23, MINUS (NUM 65, NUM 32))), ANDALSO (ORELSE (TRUE, FALSE),NOT FALSE));;
*)
