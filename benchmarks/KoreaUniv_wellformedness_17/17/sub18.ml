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
and ls = (var * exp) list

(* test cases *)
let pgm1 = LET ("x", CONST 1, VAR "x")
let pgm2 = 
  LET ("f", PROC ("x", VAR "x"), 
    IF (CALL (VAR "f", ISZERO (CONST 0)), 
        CALL (VAR "f", CONST 11), 
        CALL (VAR "f", CONST 22)))
let pgm3 = LET ("x", ADD (CONST 1, ISZERO (CONST 0)), CONST 2)


(* You can define datatypes and helper functions as necessary *)
let empty_ls = []
let rec apply : exp -> ls -> exp
=fun e l -> 
   match e with 
   | CONST n -> CONST n
   | VAR x -> (match l with
		                | [] -> VAR x
    	              | (x1,e1)::tl -> if x = x1 then e1 else apply e tl)
   | ADD (e1,e2) -> ADD((apply e1 l), (apply e2 l))
   | SUB (e1,e2) -> SUB((apply e1 l), (apply e2 l))
   | MUL (e1,e2) -> MUL((apply e1 l), (apply e2 l))
   | DIV (e1,e2) -> DIV((apply e1 l), (apply e2 l))
   | ISZERO e1 -> ISZERO (apply e1 l)
   | READ -> apply (CONST (read_int ())) l  
   | IF (e1,e2,e3) -> IF((apply e1 l),(apply e2 l),(apply e3 l))
   | LET (v,e1,e2) -> LET(v,(apply e1 l),(apply e2 l))
   | LETREC (v1,v2,e1,e2) -> LETREC(v1,v2,(apply e1 l),(apply e2 l))
   | PROC (v, e1) -> PROC (v, (apply e1 l))
   | CALL (e1,e2) -> CALL ((apply e1 l),(apply e2 l))

let rec find :var -> exp -> bool
= fun x e ->
	match e with 
	| CONST n -> false
	| VAR v -> v=x
	| ADD (e1,e2)|SUB (e1,e2)| MUL (e1,e2)|DIV (e1,e2)|LET (_,e1,e2)|LETREC (_,_,e1,e2)|CALL(e1,e2) -> (find x e1)||(find x e2)
	| ISZERO e1 |PROC(_,e1) -> find x e1
  | READ -> find x (CONST (read_int ()))
  | IF (e1,e2,e3) -> ((find x e1)||(find x e2))||(find x e3)
	

let rec expand : exp -> exp 
= fun exp -> 
	let l = empty_ls in
	match exp with
	| CONST n -> CONST n
	| VAR x -> VAR x
  | ADD (e1,e2) -> ADD ((expand e1),(expand e2))
	| SUB (e1,e2) -> SUB ((expand e1),(expand e2))
	| MUL (e1,e2) -> MUL ((expand e1),(expand e2))
	| DIV (e1,e2) -> DIV ((expand e1),(expand e2))
	| ISZERO e1 -> ISZERO (expand e1)
	| READ -> expand (CONST (read_int ()))
	| IF (e1,e2,e3) -> IF((expand e1),(expand e2),(expand e3))
	| LET (x,e1,e2) -> if (find x e2) then (if e1 = READ then apply (expand e2) ((x,CONST (read_int ()))::l) else apply (expand e2) ((x,e1)::l)) else LET(x,e1,e2)
	| LETREC (f,x,e1,e2) -> if (find f e2) then apply (expand e2) ((f,LETREC(f,x,e1,e2))::l) else LETREC(f,x,e1,e2) 
	| PROC (x,e1) -> PROC (x,(expand e1))
	| CALL (e1,e2) -> CALL ((expand e1),(expand e2))

(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string



let rec check : lambda -> bool
= fun lam ->
let rec eval : lambda -> var list -> var list
 = fun lam ll->
		match lam with 
		| V v -> (match ll with
						 | [] -> ("false")::ll
						 | hd::tl -> if hd = v then ll else hd::(eval (V v) tl))
		| P (v,l) -> let ll2 = v::ll in eval l ll2
		| C (l1,l2) ->  eval l2 (eval l1 ll)
in let rec compare : var list -> bool
 = fun ll -> 
		match ll with
		| [] -> true
		| hd::tl -> if hd = "false" then false else compare tl
in compare (eval lam [])

