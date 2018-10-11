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
= fun exp -> exp (* TODO *)

let rec expend2 : exp->exp
=fun exp ->    
    match exp with
    |VAR x ->VAR x
    |CONST n->CONST n
    |ADD (e1,e2)-> ADD(expend2 e1, expend2 e2)
    |SUB (e1,e2)-> SUB(expend2 e1, expend2 e2)    	 
    |MUL (e1,e2)-> MUL(expend2 e1, expend2 e2)    
    |DIV (e1,e2)-> DIV(expend2 e1, expend2 e2)    	 	    
    |ISZERO e-> ISZERO (expend2 e)
    |LET (v,e1,e2) -> (match e2 with
             | VAR x -> if x=v then e1 else LET (v,e1,e2)
             | CONST n -> LET (v,e1,e2)
             | _->  let rec search e2 v =
                (match e2 with
                | VAR x -> if v=x then e1 else e2
                | CONST n -> CONST n
                | ADD(e1,e2) -> ADD((search e1 v),(search e2 v))
                       | SUB(e1,e2) -> SUB((search e1 v),(search e2 v))
                | MUL(e1,e2) -> MUL((search e1 v),(search e2 v))
                | DIV(e1,e2) -> DIV((search e1 v),(search e2 v))
                | ISZERO e -> ISZERO(search e v)
                | IF(e1,e2,e3) -> IF((search e1 v),(search e2 v),(search e3 v))
                | READ -> search (CONST(read_int())) v
                | LET(v1,e1,e2) -> LET(v1,(search e1 v),(search e2 v))
                | LETREC(f, x, e1,e2)-> LETREC(f,x,(search e1 v),(search e2 v))
                | CALL (e1,e2) -> CALL((search e1 v),(search e2 v))
                | PROC (x, e) -> PROC(x,(search e v)) )in
                search e2 v)
    |LETREC (f,v,e1,e2) -> (match e2 with
             | VAR x -> if x=f then e1 else LETREC (f,x,e1,e2)
             | CONST n -> LETREC (f,v,e1,e2)
             | _->  let rec search e2 v =
                (match e2 with
                | VAR x -> if v=x then e1 else e2
                | CONST n -> CONST n
                | ADD(e1,e2) -> ADD((search e1 v),(search e2 v))
                       | SUB(e1,e2) -> SUB((search e1 v),(search e2 v))
                | MUL(e1,e2) -> MUL((search e1 v),(search e2 v))
                | DIV(e1,e2) -> DIV((search e1 v),(search e2 v))
                | ISZERO e -> ISZERO(search e v)
                | IF(e1,e2,e3) -> IF((search e1 v),(search e2 v),(search e3 v))
                | READ -> search (CONST(read_int())) v
                | LET(v1,e1,e2) -> LET(v1,(search e1 v),(search e2 v))
                | LETREC(f, x, e1,e2)-> LETREC(f,x,(search e1 v),(search e2 v))
                | CALL (e1,e2) -> CALL((search e1 v),(search e2 v))
                | PROC (x, e) -> PROC(x,(search e v)) )in
                search e2 f)  
    |IF (e1,e2,e3)->IF(expend2 e1,expend2 e2, expend2 e3)
    |PROC (v,e) -> PROC(v, expend2 e)
    |CALL (e1,e2)-> CALL(expend2 e1, expend2 e2)
    |_-> exp;;

let expand : exp->exp
=fun exp -> expend2 exp;;
(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> true (* TODO *)
     let rec check_2 (lam,l) =
    match lam with
        V v ->
        (match l with
        [] -> false
        |h::t -> if v=h then true else check_2 (V v,t))
        | P (v,lam) -> check_2 (lam, v::l)
        | C (lam1,lam2) -> if check_2 (lam1,l)=true && check_2 (lam2,l)=true then true
else false;;

    let check : lambda -> bool
    = fun lam -> check_2 (lam,[]);; 