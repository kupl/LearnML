

type exp =
| X
| INT of int
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp

let rec exptofun : exp -> (int -> int)
= fun e ->
	match e with
	| X -> (fun x -> x)
	| INT n -> (fun x -> x) 
	| ADD (e1,e2) -> (fun x -> (((exptofun e1) x) + ((exptofun e2) x)))
	| SUB (e1,e2) -> (fun x -> (((exptofun e1) x) - ((exptofun e2) x)))
	| MUL (e1,e2) -> (fun x -> (((exptofun e1) x) * ((exptofun e2) x)))
	| DIV (e1,e2) -> (fun x -> (((exptofun e1) x) / ((exptofun e2) x)))
	| _ -> (fun x -> 0)

let rec sigma : int * int * (int -> int) -> int
= fun (a,b,f) ->
	if a=b then f a
		else (f a) + (sigma (a+1,b,f));;

let rec reculse : exp -> int
= fun e ->
match e with
	| X -> raise (Failure "Error")
	| INT n -> n
	| ADD (INT n1,INT n2) -> n1+n2
	| ADD (e1, e2) -> reculse e1 + reculse e2
	| SUB (INT n1,INT n2) -> n1-n2
	| SUB (e1, e2) -> reculse e1 - reculse e2
	| MUL (INT n1,INT n2) -> n1*n2
	| MUL (e1, e2) -> reculse e1 * reculse e2
	| DIV (INT n1,INT n2) -> n1/n2
	| DIV (e1, e2) -> reculse e1 / reculse e2
	| SIGMA (INT a, INT b, e) -> sigma (a, b, (exptofun e))
	| SIGMA (e1, e2, e3) -> sigma (reculse e1, reculse e2, (exptofun e3))

let rec calculator : exp -> int
= fun exp ->
	(match exp with
	X -> 0
| INT n -> n
| ADD (e1,e2) -> (reculse e1 + reculse e2)
| SUB (e1,e2) -> (reculse e1 - reculse e2)
| MUL (e1,e2) -> (reculse e1 * reculse e2)
| DIV (e1,e2) -> (reculse e1 / reculse e2)
| SIGMA (e1,e2,e3) -> sigma (reculse e1, reculse e2, exptofun e3))