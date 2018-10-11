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
let empty_env = []
let extend_env (x,v) e = (x,v)::e

let rec occur var exp = (match exp with
| VAR x -> if x = var then true else false
| ADD (e1, e2) -> (occur var e1) || (occur var e2)
| SUB (e1, e2) -> (occur var e1) || (occur var e2)
| MUL (e1, e2) -> (occur var e1) || (occur var e2)
| DIV (e1, e2) -> (occur var e1) || (occur var e2)
| ISZERO e1 -> occur var e1
| IF (e1, e2, e3) -> (occur var e1) || (occur var e2) || (occur var e3)
| LET (v, e1, e2) -> (occur var e1) || (occur var e2)
| LETREC (v1, v2, e1, e2) -> (occur var e1) || (occur var e2)
| PROC (v, e) -> occur var e
| CALL (e1, e2) -> (occur var e1) || (occur var e2)
| _ -> false)

let rec apply_env e x =
  (match e with
  | [] -> VAR x
  | (stored_x,v)::tl -> if x = stored_x then v else apply_env tl x)

let rec solve exp env = 
  match exp with
  | CONST c -> CONST c
  | VAR x -> apply_env env x
  | ADD (e1, e2) -> ADD (solve e1 env, solve e2 env)
  | SUB (e1, e2) -> SUB (solve e1 env, solve e2 env)
  | MUL (e1, e2) -> MUL (solve e1 env, solve e2 env)
  | DIV (e1, e2) -> DIV (solve e1 env, solve e2 env)
  | ISZERO e1 -> ISZERO (solve e1 env)
  | READ -> READ (*??*)
  | IF (e1, e2, e3) -> IF(solve e1 env, solve e2 env, solve e3 env)
  | LET (v, e1, e2) -> let env2 = extend_env (v, e1) env in
                      if occur v e2 then solve e2 env2
                      else LET (v, e1, solve e2 env2) 
  | LETREC (v1, v2, e1, e2) -> LETREC (v1, v2, solve e1 env, solve e2 env) (*modify!!!!!!*)                 
  | PROC (v, e) -> PROC ("v", solve e env)
  | CALL (e1, e2) -> CALL (solve e1 env, solve e2 env)

let rec expand : exp -> exp 
= fun exp -> solve exp empty_env



(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> 
  let rec checker lamb set = let rec probe x set = 
                      match set with
                      | [] -> false
                      | hd::tl -> if x = hd then true else probe x tl in 
                         match lamb with
                          | V v -> probe v set
                          | P (v, lamb) -> let extend x set = x :: set in 
                                           let set2 = extend v set in checker lamb set2
                          | C (l1, l2) -> checker l1 set && checker l2 set 
                            in checker lam []
(*
let t1 = P ("a", V "a")
let t2 = P ("a", P ("a", V "a"))
let t3 = P ("a", P ("b", C (V "a", V "b")))
let t4 = P ("a", C (V "a", P ("b", V "a")))

let f1 = P ("a", V "b")
let f2 = P ("a", C (V "a", P ("b", V "c")))
let f3 = P ("a", P ("b", C (V "a", V "c")))

let test = LET("y", CONST 3, 
  LETREC("f", "x", 
    IF (ISZERO (VAR "x"), CONST 1, MUL( CALL (VAR "f", SUB (VAR "x", CONST 1)), VAR "x")), 
      MUL(CALL(VAR "f", CONST 3), VAR "y")))
*)