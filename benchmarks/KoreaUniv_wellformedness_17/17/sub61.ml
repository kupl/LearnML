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

exception NotDefined

(* test cases *)
let pgm1 = LET ("x", CONST 1, VAR "x")
let pgm2 = 
  LET ("f", PROC ("x", VAR "x"), 
    IF (CALL (VAR "f", ISZERO (CONST 0)), 
        CALL (VAR "f", CONST 11), 
        CALL (VAR "f", CONST 22)))
let pgm3 = LET ("x", ADD (CONST 1, ISZERO (CONST 0)), CONST 2)

(* You can define datatypes and helper functions as necessary *)

let rec eval: exp -> (var*exp) list -> exp
= fun exp lst -> match exp with
| CONST (n) -> exp
| VAR (x) -> 
(match lst with
  |[] -> exp
  |(v,e)::tl -> if (v=x) then e else (eval exp tl)
)
| ADD (e1,e2) -> ADD ((eval e1 lst),(eval e1 lst))
| SUB (e1,e2) -> SUB ((eval e1 lst),(eval e1 lst))
| MUL (e1,e2) -> MUL ((eval e1 lst),(eval e1 lst))
| DIV (e1,e2) -> DIV ((eval e1 lst),(eval e1 lst))
| ISZERO (n) -> ISZERO (eval n lst)
| READ -> READ
| IF (b,e1,e2) -> IF ((eval b lst), (eval e1 lst), (eval e2 lst))
| LET (v, e1, e2) -> (eval e2 ((v,eval e1 lst)::lst))
| LETREC (f,x,e1,e2) -> raise NotDefined
| PROC (v,e) -> 
  let rec penv: var -> (var*exp) list -> (var*exp) list
    =fun var lst -> (match lst with
    | [] -> []
    | (v,e)::tl -> if (v=var) then []@(penv var tl) else (v,e)::(penv var tl)
    )
  in PROC(v, eval e (penv v lst))
| CALL (e1,e2) -> CALL (eval e1 lst, eval e2 lst)

let rec expand : exp -> exp 
= fun exp -> match exp with
| LET (v,e1,e2) -> if ((eval exp [])=(eval e2 [])) then LET(v,e1,(expand e2)) else (eval exp [])
|_ -> (eval exp [])


(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> 
  let rec vcheck: var -> var list -> bool
  =fun v lst -> 
  (match lst with
    | [] -> false
    | hd::tl -> if (hd=v) then true else (vcheck v tl)
  ) in
    let rec vlist: lambda -> var list -> bool
    = fun lam lst ->
    (match lam with
      | V (v) -> (vcheck v lst)
      | P (v,l) -> (vlist l (v::lst))
      | C (l1, l2) -> (vlist l1 lst) && (vlist l2 lst)
    ) in
      vlist lam []

