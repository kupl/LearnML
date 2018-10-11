(**********************)
(* Problem 1*)
(**********************)

type exp = 
	| CONST of int
	| VAR of var
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| ISZERO of exp
	| READ
	| IF of exp * exp * exp
	| LET of var * exp * exp
	| LETREC of var * var * exp * exp
	| PROC of var * exp
	| CALL of exp * exp
	and var = string

(* test cases *)
let pgm1 = LET ("x", CONST 1, VAR "x")
let pgm2 = 
	LET ("f", PROC ("x", VAR "x"), 
		IF (CALL (VAR "f", ISZERO (CONST 0)), 
			CALL (VAR "f", CONST 11), 
				CALL (VAR "f", CONST 22)))
let pgm3 = LET ("x", ADD (CONST 1, ISZERO (CONST 0)), CONST 2)

(* You can define datatypes and helper functions as necessary *)
let addMem v e m = (v, e) :: m

let rec find : exp -> (string * exp) list-> exp
= fun e m ->
	match e with
		| VAR v -> (
			match m with 
			|[] -> e
			|(a, b)::tl -> if (a = v) then b else find e tl
		)
		|_ -> e


let rec expand2 : exp -> (string * exp) list -> exp 
= fun e mem ->
	match e with 
		| CONST n -> e
		| VAR v -> find (VAR v) mem
		| ADD (e1, e2) -> ADD (expand2 (find e1 mem) mem, expand2 (find e2 mem) mem)
		| SUB (e1, e2) -> SUB (expand2 (find e1 mem) mem, expand2 (find e2 mem) mem)
		| MUL (e1, e2) -> MUL (expand2 (find e1 mem) mem, expand2 (find e2 mem) mem)
		| DIV (e1, e2) -> DIV (expand2 (find e1 mem) mem, expand2 (find e2 mem) mem)
		| ISZERO e -> ISZERO (expand2 (find e mem) mem)
		| READ -> CONST (read_int())
		| IF (e1, e2, e3) -> IF (expand2 (find e1 mem) mem, expand2 (find e2 mem) mem, expand2 (find e3 mem) mem)
		| LET (v, e1, e2) -> 
			let mem2 = addMem v (expand2 (find e1 mem) mem) mem in
			if (e2 = expand2 (find e2 mem2) mem2) then LET(v, (expand2 (find e1 mem) mem), expand2 e2 mem2) else expand2 (find e2 mem2) mem2
		| LETREC (f, x, e1, e2) -> LETREC (f, x, (expand2 (find e1 mem) mem), (expand2 (find e2 mem) mem))
		| PROC (v, e) -> PROC (v, (expand2 (find e mem) mem))
		| CALL (e1, e2) -> CALL (expand2 (find e1 mem) mem, expand2 (find e2 mem) mem)

let rec expand : exp -> exp 
= fun exp -> expand2 exp []

(**********************)
(* Problem 2*)
(**********************)

type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let rec find2 v m =
	match m with 
		|[] -> false
		|hd::tl -> if (hd = v) then true else find2 v tl

let addMem2 v m = v::m

let rec check2 : lambda -> string list -> bool
= fun lam mem -> 	
	match lam with
		| V v -> find2 v mem
		| P (v, l) -> 
			let mem2 = addMem2 v mem in
			check2 l mem2
		| C (l1, l2) -> check2 l1 mem && check2 l2 mem

let rec check : lambda -> bool
= fun lam -> check2 lam []