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
= fun exp -> (*exp*) (* TODO *)
  let rec findvar v e =
    match e with
    | VAR(x) -> x = v
    | ADD(a, b) -> (findvar v a) == true || (findvar v b) == true
    | SUB(a, b) -> (findvar v a) == true || (findvar v b) == true
    | MUL(a, b) -> (findvar v a) == true || (findvar v b) == true
    | DIV(a, b) -> (findvar v a) == true || (findvar v b) == true
    | ISZERO(x) -> findvar v x
    | IF(c, t, f) -> (findvar v c) == true || ((findvar v t) == true || (findvar v f) == true)
    | LET(v_, a, b) -> v <> v_ && ((findvar v a) == true || (findvar v b) == true)
    | LETREC(v1, v2, a, b) -> (v <> v1 && v <> v2) && ((findvar v a) == true || (findvar v b == true))
    | PROC(v_, x) -> v <> v_ == false && (findvar v x) == true
    | CALL(a, b) -> (findvar v a) == true || (findvar v b) == true
    | _ -> false in
  let rec replace v e1 e2 =
    match e2 with
    | VAR(x) ->
    if x = v then e1
    else e2
    | ADD(a, b) -> ADD((replace v e1 a), (replace v e1 b))
    | SUB(a, b) -> SUB((replace v e1 a), (replace v e1 b))
    | MUL(a, b) -> MUL((replace v e1 a), (replace v e1 b))
    | DIV(a, b) -> DIV((replace v e1 a), (replace v e1 b))
    | ISZERO(x) -> ISZERO((replace v e1 x))
    | IF(c, t, f) -> IF((replace v e1 c), (replace v e1 t), (replace v e1 f))
    | LET(v_, a, b) -> LET(v_, (replace v e1 a), (replace v e1 b))
    | LETREC(v1, v2, a, b) -> LETREC(v1, v2, (replace v e1 a), (replace v e1 b))
    | PROC(v_, x) -> PROC(v_, (replace v e1 x))
    | CALL(a, b) -> CALL((replace v e1 a), (replace v e1 b))
    | _ -> e2 in
  match exp with
  | LET(v, e1, e2) ->
  let e1_ = expand e1 in
  let e2_ = expand e2 in
  if (findvar v e2_) == true then replace v e1_ e2_
  else LET(v, e1_, e2_)
  | _ -> exp


(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> (*true*) (* TODO *)
  let rec findvar l v =
    match l with
    | [] -> false
    | hd::tl ->
    if hd = v then true
    else findvar tl v in
  let rec impl l e =
    match e with
    | V v -> (findvar l v)
    | P(v, e_) -> impl (v::l) e_
    | C(e1, e2) -> (impl l e1) == true && (impl l e2) == true in
  impl [] lam
