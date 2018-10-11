
(*problem1*)

type exp = 
|CONST of int
|VAR of var 
|ADD of exp * exp
|SUB of exp * exp
|MUL of exp * exp
|DIV of exp * exp
|ISZERO of exp
|READ
|IF of exp * exp * exp
|LET of var * exp * exp
|LETREC of var * var * exp * exp
|PROC of var * exp
|CALL of exp * exp
and var = string

(*test cases*)

let pgm1 = LET("x",CONST 1, VAR "x")
let pgm2 = 
LET ("f", PROC("x", VAR "x"),
IF(CALL(VAR "f", ISZERO (CONST 0)),
CALL(VAR "f", CONST 11),
CALL(VAR"f",CONST 22)))
let pgm3 = LET ("x", ADD(CONST 1, ISZERO(CONST 0)),CONST 2)




let rec findreplace v e1 e2 =
	match e2 with
	|CONST a -> CONST a
	|VAR b -> if VAR b =  VAR v then  e1 else  VAR b
	|ADD (a,b) -> ADD(findreplace v e1 a, findreplace v e1 b)
	|SUB (a,b) -> SUB(findreplace v e1 a, findreplace v e1 b)
	|MUL (a,b) -> MUL(findreplace v e1 a, findreplace v e1 b)
	|DIV (a,b) -> DIV(findreplace v e1 a, findreplace v e1 b)
	|ISZERO e ->  ISZERO(findreplace v e1 e)
	|READ -> READ
	|IF (a, b, c) -> IF(findreplace v e1 a, 
	findreplace v e1 b, findreplace v e1 c)
	|LET (f, a, b) -> let efix = findreplace f a b in
	findreplace v e1 efix
	|LETREC(v1,v2,a,b) -> LETREC(v1,v2,a,b)
	|PROC (v1,e) -> PROC (v1, findreplace v e1 e)
	|CALL (a,b) -> CALL (findreplace v e1 a, findreplace v e1 b)

let rec findv v e1 e2 = 
	match e2 with
	|CONST a -> false
	|VAR b -> if VAR b = VAR v then true  else false
	|ADD (a,b) -> findv v e1 a || findv v e2 b
	|SUB (a,b) -> findv v e1 a || findv v e2 b
	|MUL (a,b) -> findv v e1 a || findv v e2 b
	|DIV (a,b) -> findv v e1 a || findv v e2 b
	|ISZERO e ->  findv v e1 e
	|READ -> false
	|IF (a, b, c) -> findv v e1 a|| findv v e1 b|| findv v e1 c 
	|LET (f, a, b) -> findv f a b || findv v e1 a || findv  v e1 b || if VAR f = VAR v  then true else false 
	|LETREC(v1,v2,a,b) -> if VAR v1 = VAR v then true else false || if VAR v2 = VAR v then true else false
	||findv  v e1 a || findv  v e1 b
	|PROC (v1,e) -> if VAR v1= VAR v then true else false ||findv v e1 e
	|CALL (a,b) -> findv v e1 a || findv v e1 b 

let rec expand : exp -> exp
= fun e ->
match e with
|CONST a -> CONST a
|VAR v -> VAR v
|ADD (e1,e2) -> ADD(expand e1, expand e2)
|SUB (e1,e2) -> SUB(expand e1, expand e2)
|MUL (e1,e2) -> MUL(expand e1, expand e2)
|DIV (e1,e2) -> DIV(expand e1, expand e2)
|ISZERO e -> ISZERO (expand e)
|READ -> READ
|IF (e1, e2, e3) -> IF(expand e1, expand e2, expand e3)
|LET (v, e1, e2) -> let a = findv v e1 e2 in
if a = true then findreplace v e1 e2 else LET(v, expand e1, expand e2)
|LETREC(v1,v2,e1,e2) -> LETREC(v1, v2 , expand e1, expand e2)
|PROC (v,e) -> PROC(v,expand e)
|CALL (e1,e2) -> CALL (expand e1, expand e2)


(*problem 2*)
type lambda = 
	|V of var
	|P of var * lambda
	|C of lambda * lambda


let lam1 = P("a", V"a")
let lam2 = P("a", P("a", V"a"))
let lam3 = P("a",V"b")
let lam4 = P("a",C(V"a",P("b",V"c")))

let rec classify lam li =
match lam with
|P (v,l) -> classify l (li @ [v])
|C(l1,l2) -> classify l1 li && classify l2 li
|V a -> 
(match li with
|[]->false
|hd::tl -> if hd = a then true else classify lam tl
)

let rec check : lambda -> bool
= fun lam ->  
	classify lam [] 