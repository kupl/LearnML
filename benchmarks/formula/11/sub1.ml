(* 2004-11951 Noh, Soon Hyun *)

(* skeleton type from TA *)
type expr =
	NUM of int | PLUS of expr * expr
	| MINUS of expr * expr
type formula =
 	TRUE | FALSE | NOT of formula | ANDALSO of formula * formula
	| ORELSE of formula * formula | IMPLY of formula * formula
	| LESS of expr * expr

(* to evaluete expressions *)
let rec evalexpr v =
	match v with
	| NUM x -> x
	| PLUS (x, y) -> (evalexpr x) + (evalexpr y)
	| MINUS (x, y) -> (evalexpr x) - (evalexpr y)

(* middle-level function of eval *)
(* If I use <formula> -> <bool> function direct to recursion,
I cannot use return value to its argument again *)
(* type of protoeval f is <formula> -> <formula> *)
let rec protoeval f =
	match f with
	| TRUE -> TRUE
	| FALSE -> FALSE
	| (NOT (TRUE)) -> FALSE
	| (NOT (FALSE)) -> TRUE
	(* handle case when there are more formulas in x *)
	| (NOT (x)) -> (protoeval (NOT (protoeval x)))
	| (ANDALSO (FALSE, FALSE)) -> FALSE
	| (ANDALSO (TRUE, FALSE)) -> FALSE
	| (ANDALSO (FALSE, TRUE)) -> FALSE
	| (ANDALSO (TRUE, TRUE)) -> TRUE
	| (ANDALSO (x, y)) 
	-> (protoeval (ANDALSO((protoeval x), (protoeval y))))
	| (ORELSE (FALSE, FALSE)) -> FALSE
	| (ORELSE (TRUE, FALSE)) -> TRUE
	| (ORELSE (FALSE, TRUE)) -> TRUE
	| (ORELSE (TRUE, TRUE)) -> TRUE
	| (ORELSE (x, y)) 
	-> (protoeval (ORELSE((protoeval x), (protoeval y))))
	| (IMPLY (FALSE, FALSE)) -> TRUE
	| (IMPLY (TRUE, FALSE)) -> FALSE
	| (IMPLY (FALSE, TRUE)) -> TRUE
	| (IMPLY (TRUE, TRUE)) -> TRUE
	| (IMPLY (x, y)) -> (protoeval (IMPLY((protoeval x), (protoeval y))))
	| LESS (a, b) -> if (evalexpr(a) < evalexpr(b)) then TRUE else FALSE

(* main function *)
let eval f =
	if (protoeval f)=TRUE then true
	else if (protoeval f)=FALSE then false
	else (print_string"error\n"; false)

(* Test Code :: some parts are refrenced from last semester class web board
let test = eval(LESS(PLUS(NUM 5, NUM 5), MINUS(NUM 20, NUM 13)))
let print_bool a =
	if a=true then print_string "true\n"
	else print_string "false\n"

let f p q = eval (ANDALSO (p, (ORELSE (q, (ANDALSO ((NOT p), (NOT q)))))));; 
let r_f p q = not (f p q);;
let _ = print_bool (f TRUE TRUE) 
let _ = print_bool (f TRUE FALSE) 
let _ = print_bool (f FALSE TRUE) 
let _ = print_bool (f FALSE FALSE) 
let _ = print_bool (r_f TRUE TRUE) 
let _ = print_bool (r_f TRUE FALSE) 
let _ = print_bool (r_f FALSE TRUE) 
let _ = print_bool (r_f FALSE FALSE)

let _ = print_bool test
*)
