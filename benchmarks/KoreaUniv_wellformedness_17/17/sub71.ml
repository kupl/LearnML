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
let rec helper : (exp * bool)-> var -> bool
= fun (exp, b) v ->
  match exp with
  | CONST n -> b
  | VAR x -> 
    if (v=x) then true
    else b
  | ADD (e1, e2) -> (helper (e1, b) v ) || (helper (e2, b) v)
  | SUB (e1, e2) -> (helper (e1, b) v ) || (helper (e2, b) v)
  | MUL (e1, e2) -> (helper (e1, b) v ) || (helper (e2, b) v)
  | DIV (e1, e2) -> (helper (e1, b) v ) || (helper (e2, b) v)
  | ISZERO e -> helper (e, b) v
  | IF (e1, e2, e3) -> ((helper (e1, b) v) || (helper (e2, b) v))||(helper (e3, b) v)
  | LET (x, e1, e2) -> (helper (e1, b) v) || (helper (e2, b) v)
  | LETREC (f, x, e1, e2) -> (helper (e1, b) v) || (helper (e2, b) v)
  | PROC (v, e) -> helper (e, b) v
  | CALL (e1, e2) -> (helper (e1, b) v) || (helper (e2, b) v)
(* You can define datatypes and helper functions as necessary *)
let rec expand : exp -> exp 
= fun exp -> 
  let rec expander : exp  -> mem -> exp 
  = fun exp1 mem ->
  match exp1 with
  | CONST n -> CONST n
  | VAR x -> 
    if (findmem x mem) then
      match mem with
      | (v, l)::tl -> 
          if (x=v) then l
          else expander (VAR x) tl
    else VAR x
  | ADD (e1, e2) -> ADD ((expander e1 mem), (expander e2 mem))
  | SUB (e1, e2) -> SUB ((expander e1 mem), (expander e2 mem))
  | MUL (e1, e2) -> MUL ((expander e1 mem), (expander e2 mem))
  | DIV (e1, e2) -> DIV ((expander e1 mem), (expander e2 mem))
  | ISZERO e -> (ISZERO (expander e mem))
  | READ ->  
    let n = read_int() in
      CONST n
  | IF (e1, e2, e3) -> IF ((expander e1 mem), (expander e2 mem), (expander e3 mem))
  | LET (x, e1, e2) -> 
    if (helper (e2, false) x) then  
      let newmem = extend_mem (x, e1) mem in
        expander e2 newmem
    else LET (x, expander e1 mem, expander e2 mem)
  | LETREC (f, x, e1, e2) ->
    if (helper (e2, false) f) then
      let newmem = extend_mem (f, e1) mem in
        expander e2 newmem
    else LETREC (f, x, expander e1 mem, expander e2 mem)
  | PROC (v, e) -> PROC (v, (expander e mem))
  | CALL (e1, e2)-> CALL ((expander e1 mem), (expander e2 mem))
  | _ -> exp in expander exp []


(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam ->
  let rec checker : lambda -> var list -> bool -> bool
  = fun lam1 lst b ->
  match lam1 with 
  | V x -> 
  (let rec help : var-> var list -> bool
  = fun v vlst ->
    (match vlst with
    |[] -> false
    |hd::tl -> if (v=hd) then true else help v tl) in help x lst)
  | P (v, l) -> (checker l (v::lst) b) && b 
  | C (l1, l2) -> ((checker l1 lst b) && (checker l2 lst b)) && b in checker lam [] true

