(**********************) (* Problem 1 *) (**********************) 
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
type mem = (var * exp) list

(* test cases *) 
let pgm1 = LET ("x", CONST 1, VAR "x") 
let pgm2 = LET ("f", PROC ("x", VAR "x"), IF (CALL (VAR "f", ISZERO (CONST 0)), CALL (VAR "f", CONST 11), CALL (VAR "f", CONST 22))) 
let pgm3 = LET ("x", ADD (CONST 1, ISZERO (CONST 0)), CONST 2) 
let pgm4 = LET ("x", CONST 1, LET("y", CONST 2, ADD(VAR "x", VAR "y")))
let pgm5 = LET ("x", CONST 1, LET("y", CONST 2, VAR "y"))

let empty_mem = []
let extend_mem (v,e) m = (v,e)::m
let rec findmem v m =
	match m with
	| [] -> false
	| (x,e)::tl -> if (v = x) then true else (findmem v tl)

let rec checker : (exp * bool)-> var -> bool
= fun (exp, b) v ->
  match exp with
  | CONST n -> b
  | VAR x -> if (v=x) then true
      		 else b
  | ADD (e1, e2) -> (checker (e1, b) v ) || (checker (e2, b) v)
  | SUB (e1, e2) -> (checker (e1, b) v ) || (checker (e2, b) v)
  | MUL (e1, e2) -> (checker (e1, b) v ) || (checker (e2, b) v)
  | DIV (e1, e2) -> (checker (e1, b) v ) || (checker (e2, b) v)
  | ISZERO e -> checker (e, b) v
  | IF (e1, e2, e3) -> ((checker (e1, b) v) || (checker (e2, b) v))||(checker (e3, b) v)
  | LET (v1, e1, e2) -> (checker (e1, b) v) || (checker (e2, b) v)
  | LETREC (v1, v2, e1, e2) -> (checker (e1, b) v) || (checker (e2, b) v)
  | PROC (v, e) -> checker (e, b) v
  | CALL (e1, e2) -> (checker (e1, b) v) || (checker (e2, b) v)

let rec expand2 : exp -> mem -> exp
=fun exp mem -> match exp with
| CONST n -> CONST n 
| VAR x -> 
  if (findmem x mem) then
    begin match mem with
    | (v, l)::tl -> 
        if (x=v) then l
         else expand2 (VAR x) tl end
  else VAR x
| ADD (e1, e2) -> ADD(expand2 e1 mem, expand2 e2 mem)
| SUB (e1, e2) -> SUB(expand2 e1 mem, expand2 e2 mem)
| MUL (e1, e2) -> MUL(expand2 e1 mem, expand2 e2 mem)
| DIV (e1, e2) -> DIV(expand2 e1 mem, expand2 e2 mem)
| ISZERO e -> ISZERO (expand2 e mem)
| READ -> let v = read_int() in (CONST v)
| IF (e1,e2,e3) -> IF(expand2 e1 mem, expand2 e2 mem, expand2 e3 mem)
| LET (v1, e1, e2) -> if(checker (e2, false) v1) then let mem2 = extend_mem (v1,e1) mem in
					  expand2 e2 mem2
					  else LET(v1, expand2 e1 mem, expand2 e2 mem)
| LETREC (v1,v2,e1,e2) -> let mem2 = extend_mem (v1, VAR v2) mem in
						  let mem3 = extend_mem (v2, e1) mem2 in
						  expand2 e2 mem3
| PROC (v, e) -> begin match (expand2 (VAR v) mem) with
                 | VAR v1 -> PROC(v1, expand2 e mem) end
| CALL (e1, e2) -> CALL (expand2 e1 mem, expand2 e2 mem)
| _ -> exp

(* You can define datatypes and helper functions as necessary *) 
let expand : exp -> exp 
= fun exp -> 
expand2 exp [] (* TODO *) (**********************) 



(* Problem 2 *) (**********************) 
type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec helper : var -> var list -> bool
= fun v vlist -> match vlist with
| [] -> false
| hd::tl -> if (v=hd) then true else helper v tl

let rec checker : lambda -> var list -> bool -> bool
= fun l lst b -> match l with
| V x -> helper x lst
| P (v1, l1) -> (checker l1 (v1::lst) b) && b
| C (l1, l2) -> ((checker l1 lst b) && (checker l2 lst b)) && b

let rec check : lambda -> bool
= fun l -> checker l [] true