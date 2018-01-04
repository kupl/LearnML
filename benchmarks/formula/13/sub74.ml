(* file name : ex2.ml *)
(* author : Jisoon Park (jspark@ropas.snu.ac.kr) *)
(* date : 2013-09-13 *)
(* Exercise 2 *)
type formula = TRUE
						| FALSE
						| NOT of formula
						| ANDALSO of formula * formula
						| ORELSE of formula * formula
						| IMPLY of formula * formula
						| LESS of expr * expr
and expr = NUM of int
					| PLUS of expr * expr
					| MINUS of expr * expr

let rec calc : expr -> int
	= fun exp ->
		match exp with
		| NUM n -> n
		| PLUS (e1, e2) -> calc e1 + calc e2
		| MINUS (e1, e2) -> calc e1 - calc e2

let rec eval : formula -> bool
	= fun frml ->
		match frml with
		| TRUE -> true
		| FALSE -> false
		| NOT f -> if (eval f) then false else true
		| ANDALSO (f1, f2) -> eval f1 && eval f2
		| ORELSE (f1, f2) -> eval f1 || eval f2
		| IMPLY (f1, f2) -> if eval f2 then true else if eval f1 then false else true
		| LESS (e1, e2) -> if (calc e1 < calc e2) then true else false
