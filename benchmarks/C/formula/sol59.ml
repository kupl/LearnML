(* 2004-11951 Noh, Soon Hyun *)

(* skeleton type from TA *)
type exp =
	Num of int | Plus of exp * exp
	| Minus of exp * exp
type formula =
 	True | False | Not of formula | AndAlso of formula * formula
	| OrElse of formula * formula | Imply of formula * formula
	| Equal of exp * exp

(* to evaluete expessions *)
let rec evalexp v =
	match v with
	| Num x -> x
	| Plus (x, y) -> (evalexp x) + (evalexp y)
	| Minus (x, y) -> (evalexp x) - (evalexp y)

(* middle-level function of eval *)
(* If I use =formula> -> =bool> function direct to recursion,
I cannot use return value to its argument again *)
(* type of protoeval f is =formula> -> =formula> *)
let rec protoeval f =
	match f with
	| True -> True
	| False -> False
	| (Not (True)) -> False
	| (Not (False)) -> True
	(* handle case when there are more formulas in x *)
	| (Not (x)) -> (protoeval (Not (protoeval x)))
	| (AndAlso (False, False)) -> False
	| (AndAlso (True, False)) -> False
	| (AndAlso (False, True)) -> False
	| (AndAlso (True, True)) -> True
	| (AndAlso (x, y)) 
	-> (protoeval (AndAlso((protoeval x), (protoeval y))))
	| (OrElse (False, False)) -> False
	| (OrElse (True, False)) -> True
	| (OrElse (False, True)) -> True
	| (OrElse (True, True)) -> True
	| (OrElse (x, y)) 
	-> (protoeval (OrElse((protoeval x), (protoeval y))))
	| (Imply (False, False)) -> True
	| (Imply (True, False)) -> False
	| (Imply (False, True)) -> True
	| (Imply (True, True)) -> True
	| (Imply (x, y)) -> (protoeval (Imply((protoeval x), (protoeval y))))
	| Equal (a, b) -> if (evalexp(a) = evalexp(b)) then True else False

(* main function *)
let eval f =
	if (protoeval f)=True then true
	else if (protoeval f)=False then false
	else (false)

(* Test Code :: some parts are refrenced from last semester class web board
let test = eval(Equal(Plus(Num 5, Num 5), Minus(Num 20, Num 13)))
let print_bool a =
	if a=true then print_string "true\n"
	else print_string "false\n"

let f p q = eval (AndAlso (p, (OrElse (q, (AndAlso ((Not p), (Not q)))))));; 
let r_f p q = not (f p q);;
let _ = print_bool (f True True) 
let _ = print_bool (f True False) 
let _ = print_bool (f False True) 
let _ = print_bool (f False False) 
let _ = print_bool (r_f True True) 
let _ = print_bool (r_f True False) 
let _ = print_bool (r_f False True) 
let _ = print_bool (r_f False False)

let _ = print_bool test
*)
