(*	Problem 1	*)

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
let pgm3 = LET ("x", ADD(CONST 1, ISZERO (CONST 0)), CONST 2)

let rec expand : exp -> exp
= fun exp ->
	let rec varappend : exp -> var * exp -> exp * bool
		= fun e2 (x, e1) ->
			(match e2 with
				| CONST a -> (e2, false)
				| VAR v -> if v = x then (e1, true) else (e2, false)
				| ADD (e3, e4) ->
					let (e3', b3) = varappend e3 (x, e1) in
					let (e4', b4) = varappend e4 (x, e1) in
					(ADD (e3', e4'), b3 || b4)
				| SUB (e3, e4) ->
					let (e3', b3) = varappend e3 (x, e1) in
					let (e4', b4) = varappend e4 (x, e1) in
					(SUB (e3', e4'), b3 || b4) 
				| MUL (e3, e4) ->
					let (e3', b3) = varappend e3 (x, e1) in
					let (e4', b4) = varappend e4 (x, e1) in
					(MUL (e3', e4'), b3 || b4)
				| DIV (e3, e4) ->
					let (e3', b3) = varappend e3 (x, e1) in
					let (e4', b4) = varappend e4 (x, e1) in
					(DIV (e3', e4'), b3 || b4)
				| ISZERO (e) ->
					let (e', b) = varappend e (x, e1) in
					(ISZERO (e'), b)
				| READ -> (READ, false)
				| IF (e3, e4, e5) ->
					let (e3', b3) = varappend e3 (x, e1) in
					let (e4', b4) = varappend e4 (x, e1) in
					let (e5', b5) = varappend e5 (x, e1) in
					(IF (e3', e4', e5'), b3 || b4 || b5)
				| LET (v, e3, e4) -> let e = expand e2 in
					if e = e2 then
						let (e3', b3) = varappend e3 (x, e1) in
						let (e4', b4) = varappend e4 (x, e1) in
						(LET (v, e3', e4'), b3 || b4)
					else varappend e (x, e1) 
				| LETREC (f, v, e3, e4) ->
					let (e3', b3) = varappend e3 (x, e1) in
					let (e4', b4) = varappend e4 (x, e1) in
					(LETREC (f, v, e3', e4'), b3 || b4)
				| PROC (v, e) -> let (e', b) = varappend e (x, e1) in
 					(PROC (v, e'), b)
				| CALL (e3, e4) ->
					let (e3', b3) = varappend e3 (x, e1) in
					let (e4', b4) = varappend e4 (x, e1) in
					(CALL (e3', e4'), b3 || b4)
			) in
	match exp with
		| LET (x, e1, e2) -> let e1' = expand e1 in
			let (e2', b) = varappend e2 (x, e1') in
			if b then e2' else LET (x, e1', e2)
		| _ -> exp


(*	Problem 2	*)

type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let check :  lambda -> bool
= fun lam ->
	let rec reccheck : lambda -> var list -> bool
	= fun l vlist ->
		let rec listfind : var -> var list -> bool
		= fun x lst ->
			(match lst with
				| [] -> false
				| hd::tl -> if hd = x then true else listfind x tl
			)
		in
		match l with
			| V v -> listfind v vlist
			| P (v, l') -> reccheck l' (v::vlist)
			| C (l1, l2) -> (reccheck l1 vlist) && (reccheck l2 vlist)
	in
	reccheck lam []

