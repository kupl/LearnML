(* file name : ex2.ml *)
(* author : Jisoon Park (jspark@ropas.snu.ac.kr) *)
(* date : 2013-09-13 *)
(* Exercise 2 *)
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

let rec calc : exp -> int
	= fun exp ->
		match exp with
		| Num n -> n
		| Plus (e1, e2) -> calc e1 + calc e2
		| Minus (e1, e2) -> calc e1 - calc e2

let rec eval : formula -> bool
	= fun frml ->
		match frml with
		| True -> true
		| False -> false
		| Not f -> if (eval f) then false else true
		| AndAlso (f1, f2) -> eval f1 && eval f2
		| OrElse (f1, f2) -> eval f1 || eval f2
		| Imply (f1, f2) -> if eval f2 then true else if eval f1 then false else true
		| Equal (e1, e2) -> if (calc e1 = calc e2) then true else false
