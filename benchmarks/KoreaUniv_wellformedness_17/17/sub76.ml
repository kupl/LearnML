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

type mem = (var * exp) list
let empty_mem = []
let extend_mem (l,v) m = (l,v)::m
let rec findmem v m =
    (match m with
    | [] -> false
    | (l,s)::tl -> if (v = l) then true else (findmem v tl))
let rec helpfunc : (exp * bool)-> var -> bool
= fun (exp, b) v ->
  match exp with
  | ADD (e1, e2) -> (helpfunc (e1, b) v ) || (helpfunc (e2, b) v)
  | SUB (e1, e2) -> (helpfunc (e1, b) v ) || (helpfunc (e2, b) v)
  | MUL (e1, e2) -> (helpfunc (e1, b) v ) || (helpfunc (e2, b) v)
  | DIV (e1, e2) -> (helpfunc (e1, b) v ) || (helpfunc (e2, b) v)
  | CONST n -> b
  | VAR x -> if (v=x) then true else b
  | PROC (v, e) -> helpfunc (e, b) v
  | CALL (e1, e2) -> (helpfunc (e1, b) v) || (helpfunc (e2, b) v)
  | ISZERO e -> helpfunc (e, b) v
  | IF (e1, e2, e3) -> ((helpfunc (e1, b) v) || (helpfunc (e2, b) v))||(helpfunc (e3, b) v)
  | LET (x, e1, e2) -> (helpfunc (e1, b) v) || (helpfunc (e2, b) v)
  | LETREC (f, x, e1, e2) -> (helpfunc (e1, b) v) || (helpfunc (e2, b) v)

(* You can define datatypes and helper functions as necessary *)
let rec expand : exp -> exp 
= fun exp -> 
  let rec expand2 : exp  -> mem -> exp 
  = fun exp1 mem ->
  match exp1 with
  | CONST n -> CONST n
  | VAR x -> 
    if (findmem x mem) then
      match mem with
      | (v, l)::tl -> 
          if (x=v) then l
          else expand2 (VAR x) tl
    else VAR x
  | ADD (e1, e2) -> ADD ((expand2 e1 mem), (expand2 e2 mem))
  | SUB (e1, e2) -> SUB ((expand2 e1 mem), (expand2 e2 mem))
  | MUL (e1, e2) -> MUL ((expand2 e1 mem), (expand2 e2 mem))
  | DIV (e1, e2) -> DIV ((expand2 e1 mem), (expand2 e2 mem))
  | ISZERO e -> (ISZERO (expand2 e mem))
  | READ -> let n = read_int() in CONST n
  | IF (e1, e2, e3) -> IF ((expand2 e1 mem), (expand2 e2 mem), (expand2 e3 mem))
  | LET (x, e1, e2) -> 
    if (helpfunc (e2, false) x) then  
      let newmem = extend_mem (x, e1) mem in expand2 e2 newmem
    else LET (x, expand2 e1 mem, expand2 e2 mem)
  | LETREC (f, x, e1, e2) ->
    if (helpfunc (e2, false) f) then
      let newmem = extend_mem (f, e1) mem in expand2 e2 newmem
    else LETREC (f, x, expand2 e1 mem, expand2 e2 mem)
  | PROC (v, e) -> 
    (match (expand2 (VAR v) mem) with 
    |VAR v1 -> (PROC (v1, (expand2 e mem))))
  | CALL (e1, e2)-> CALL ((expand2 e1 mem), (expand2 e2 mem)) in expand2 exp []


(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam ->
  let rec check2 : lambda -> var list -> bool -> bool
  = fun lamb lst b ->
  match lamb with 
  | V x -> 
  (let rec helpfunc2 : var-> var list -> bool
  = fun v vlst ->
    (match vlst with
    |[] -> false
    |hd::tl -> if (v=hd) then true else helpfunc2 v tl) in helpfunc2 x lst)
  | P (v, l) -> (check2 l (v::lst) b) && b 
  | C (l1, l2) -> ((check2 l1 lst b) && (check2 l2 lst b)) && b in check2 lam [] true