(**********************) 
(* Problem 1 *) 
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
| CALL of exp * exp and var = string 
(* test cases *) 
let pgm1 = LET ("x", CONST 1, VAR "x") 
let pgm2 = LET ("f", PROC ("x", VAR "x"), 
	IF (CALL (VAR "f", ISZERO (CONST 0)), 
		CALL (VAR "f", CONST 11), CALL (VAR "f", CONST 22))) 
let pgm3 = LET ("x", ADD (CONST 1, ISZERO (CONST 0)), CONST 2) 
(* You can define datatypes and helper functions as necessary *)

let rec check x exp1 exp2 =
match exp2 with
|CONST n -> CONST n
|VAR v -> if v = x then exp1 else exp2      
|ADD(e1,e2) -> ADD((check x exp1 e1),(check x exp1 e2))
|SUB(e1,e2) -> SUB((check x exp1 e1),(check x exp1 e2))
|MUL(e1,e2) -> MUL((check x exp1 e1),(check x exp1 e2))
|DIV(e1,e2) -> DIV((check x exp1 e1),(check x exp1 e2))
|IF(e1,e2,e3) -> IF((check x exp1 e1),(check x exp1 e2),(check x exp1 e3))
|LET(x1, e1, e2) -> LET(x1, (check x exp1 e1), (check x exp1 e2))
|LETREC(f, x1, e1, e2) -> LETREC(f, x1, (check x exp1 e1), (check x exp1 e2))
|ISZERO exp -> ISZERO(check x exp1 exp)
|READ -> check x exp1 (CONST (read_int()))
|PROC(v,e1) -> PROC(v,(check x exp1 e1))
|CALL(e1,e2) -> CALL((check x exp1 e1),(check x exp1 e2)) 

let rec expand : exp -> exp 
 = fun exp -> 
 match exp with
 |CONST n -> CONST n
 |VAR x -> VAR x
 |ADD(exp1,exp2) -> ADD((expand exp1),(expand exp2))
 |SUB(exp1,exp2) -> SUB((expand exp1),(expand exp2))
 |MUL(exp1,exp2) -> MUL((expand exp1),(expand exp2))
 |DIV(exp1,exp2) -> DIV((expand exp1),(expand exp2))
 |ISZERO exp -> ISZERO(expand exp)
 |READ -> expand(CONST (read_int()))
 |IF (e1,e2,e3) -> IF(expand e1,expand e2,expand e3)
 |LET (x, exp1, exp2) -> (match exp2 with
 						|VAR v -> if v = x then exp1 else LET(x, exp1, exp2)
 						|CONST n -> LET(x,exp1,exp2)
 						|ADD(_,_)|SUB(_,_)|MUL(_,_)|DIV(_,_)|ISZERO(_)|READ|IF(_,_,_)|LET(_,_,_)|LETREC(_,_,_,_)|PROC(_,_)|CALL(_,_) -> (check x exp1 exp2)) 		
 |LETREC (f,x,e1,e2) -> (match e2 with
 						|VAR v -> if v=f then e1 else LETREC(f,x, e1, e2)
 						|CONST n -> LETREC(f,x,e1,e2)
 						|ADD(_,_)|SUB(_,_)|MUL(_,_)|DIV(_,_)|ISZERO(_)|READ|IF(_,_,_)|    LET(_,_,_)|LETREC(_,_,_,_)|PROC(_,_)|CALL(_,_) -> (check x e1 e2))
 |PROC(x,exp1) -> PROC(x,(expand exp1))
 |CALL(exp1,exp2) -> CALL ((expand exp1),(expand exp2))
 (* TODO *) 
 (**********************) 
 (* Problem 2 *) 
 (**********************) 
type lambda = 
 V of var 
 | P of var * lambda 
 | C of lambda * lambda 
 and var = string 
(* TODO *) 
let rec vcheck : lambda * (var list) -> bool
 = fun (lam, clist)  ->
 match lam with
 |V var -> (match clist with
 	|hd :: tl -> if var = hd then true else vcheck(V var, tl)
 	|[] -> false)
 |P (var, lam1) -> vcheck(lam1, var::clist)
 |C (lam1, lam2) -> vcheck(lam1, clist) && vcheck(lam2, clist)


let rec check : lambda -> bool 
 = fun lam -> vcheck(lam, []);;

