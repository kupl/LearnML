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
= fun exp -> 
  let rec change : exp -> exp -> exp -> exp
  = fun v s func ->
    match func with
    | CONST a -> func
    | VAR a -> 
      begin
        match v with
        | VAR b -> if a=b then s else VAR(a)
        | _ -> VAR(a)
      end
    | ADD(a,b) -> ADD(change v s a, change v s b)
    | SUB(a,b) -> SUB(change v s a, change v s b)
    | MUL(a,b) -> MUL(change v s a, change v s b)
    | DIV(a,b) -> DIV(change v s a, change v s b)
    | ISZERO a -> ISZERO(change v s a)
    | READ -> READ
    | IF(a,b,c) -> IF(change v s a, change v s b, change v s c)
    | LET(a,b,c) -> change v s (expand func)
    | LETREC(a,b,c,d) -> change v s (expand func)
    | PROC(a,b) -> PROC(a,change v s b)
    | CALL(a,b) -> 
      begin
        match v with
        | CALL(k,l) -> if a==k && b==l then s else CALL(change v s a,change v s b)
        | _ -> CALL(change v s a,change v s b)
      end
  in

  let rec findvar : exp -> exp -> bool
  = fun v s->
    match s with
    | CONST a -> false
    | VAR a ->
      begin
        match v with
        | VAR b -> a=b
        | _ -> false
      end
    | ADD(a,b) -> findvar v a || findvar v b
    | SUB(a,b) -> findvar v a || findvar v b
    | MUL(a,b) -> findvar v a || findvar v b
    | DIV(a,b) -> findvar v a || findvar v b
    | ISZERO a -> findvar v a
    | READ -> false
    | IF(a,b,c) -> findvar v a || findvar v b || findvar v c
    | LET(a,b,c) -> findvar v (expand (LET(a,b,c)))
    | LETREC(a,b,c,d) -> findvar v (expand (LETREC(a,b,c,d)))
    | PROC(a,b) -> findvar v (VAR(a)) || findvar v b
    | CALL(a,b) -> findvar v a || findvar v b
  in

    match exp with
    | CONST a -> CONST a
    | VAR a -> VAR a
    | ADD(a,b) -> ADD(expand a, expand b)
    | SUB(a,b) -> SUB(expand a, expand b)
    | MUL(a,b) -> MUL(expand a, expand b)
    | DIV(a,b) -> DIV(expand a, expand b)
    | ISZERO a -> ISZERO(expand a)
    | READ -> READ
    | IF(a,b,c) -> IF(expand a, expand b, expand c)
    | LET(k,e1,e2) -> 
      begin
        if findvar (VAR(k)) e2 then change (VAR(k)) e1 e2 else LET(k,e1,e2)
      end
    | LETREC(f,k,e1,e2)->
      begin
        if findvar (CALL(VAR(f),VAR(k))) e2 then change (CALL(VAR(f),VAR(k))) e1 e2 else LETREC(f,k,e1,e2)
      end
    | PROC(a,b)-> PROC(a, expand b)
    | CALL(a,b)-> CALL(expand a, expand b)


(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> 

  let rec pop_value : var list -> var -> var list
  = fun l v->
    match l with
    | [] -> []
    | hd::tl -> if hd=v then (pop_value tl v) else hd::(pop_value tl v)
  in

  let rec free_value : lambda -> var list
  = fun l->
    match l with
    | V(v) -> [v]
    | P(v0, l0) -> pop_value (free_value l0) v0
    | C(l1, l2) -> (free_value l1)@(free_value l2)
  in

  (free_value lam)==[]

