(**********************)
(*   Problem 1        *)
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
let rec expand : exp -> exp 
= fun exp -> (* TODO *)
	let rec appear : exp -> var -> bool
	= fun exp x ->
		match exp with
		| CONST i -> false
		| VAR v -> if x=v then true else false
		| ADD (e1,e2) -> appear e1 x || appear e2 x
		| SUB (e1,e2) -> appear e1 x || appear e2 x
		| MUL (e1,e2) -> appear e1 x || appear e2 x
		| DIV (e1,e2) -> appear e1 x || appear e2 x
		| ISZERO e0 -> appear e0 x
		| READ -> false
		| IF (e1,e2,e3) -> appear e1 x || appear e2 x || appear e3 x
		| LET (v,e1,e2) -> appear e1 x || appear e2 x
		| LETREC (v1,v2,e1,e2) -> appear e1 x || appear e2 x
		| PROC (v,e0) -> appear e0 x
		| CALL (e1,e2) -> appear e1 x || appear e2 x
	in
	let rec replace : exp -> var -> exp -> exp
	= fun exp x e ->
		match exp with
		| CONST i -> CONST i
		| VAR v -> if x=v then e else VAR v
		| ADD (e1,e2) -> ADD(replace e1 x e,replace e2 x e)
		| SUB (e1,e2) -> SUB(replace e1 x e,replace e2 x e)
		| MUL (e1,e2) -> MUL(replace e1 x e,replace e2 x e)
		| DIV (e1,e2) -> DIV(replace e1 x e,replace e2 x e)
		| ISZERO e0 -> ISZERO (replace e0 x e)
		| READ -> READ
		| IF (e1,e2,e3) -> IF(replace e1 x e,replace e2 x e,replace e3 x e)
		| LET (v,e1,e2) -> if v=x && appear e2 v then LET(v,e1,e2) else LET(v,replace e1 x e,replace e2 x e)
		| LETREC (v1,v2,e1,e2) -> if v1=x && appear e2 v1 then LETREC(v1,v2,e1,e2) else LETREC(v1,v2,replace e1 x e,replace e2 x e)
		| PROC (v,e0) -> PROC(v,replace e0 x e)
		| CALL (e1,e2) -> CALL(replace e1 x e,replace e2 x e)
	in
	match exp with
	| CONST i -> CONST i
	| VAR v -> VAR v
	| ADD (e1,e2) -> ADD(expand e1,expand e2)
	| SUB (e1,e2) -> SUB(expand e1,expand e2)
	| MUL (e1,e2) -> MUL(expand e1,expand e2)
	| DIV (e1,e2) -> DIV(expand e1,expand e2)
	| ISZERO e0 -> ISZERO ( expand e0 )
	| READ -> READ
	| IF (e1,e2,e3) -> IF(expand e1,expand e2,expand e3)
	| LET (v,e1,e2) -> 
		if appear e2 v then expand @@ replace e2 v e1 else LET(v,e1,e2)
	| LETREC (v1,v2,e1,e2) -> 
		if appear e1 v1 then LETREC(v1,v2,e1,e2)
		else expand @@ LET(v1,PROC(v2,e1),e2)
		(*LETREC(v1,v2,expand e1,expand e2)*)
	| PROC (v,e0) -> PROC(v,expand e0)
	| CALL (e1,e2) -> CALL(expand e1,expand e2)



(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> (* TODO *)
	let rec chk lam scp =
		match lam with
		| V x ->
		begin
			try
				(fun _ -> true) @@ List.find (fun y -> if x=y then true else false) scp
			with
				_ -> false	
		end
		| P (x,l) -> chk l (x::scp)
		| C (l1,l2) ->
			chk l1 scp && chk l2 scp
	in
	chk lam []
