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
let pgm4 = LET ("x", CONST 2, LET ("x", CONST 3, MUL (VAR "x", CONST 4)))
let pgm5 = LET ("x", CONST 2, LET ("x", MUL (VAR "x", CONST 3), MUL (VAR "x", CONST 4)))
let pgm6 = LET ("a", ADD (CONST 1, CONST 2), LET ("b", ADD (VAR "a", VAR "a"), LET ("c", ADD (VAR "b", VAR "b"), VAR "c")))
let pgm7 = LET ("base", CONST 4, LETREC ("f", "x", IF (ISZERO (VAR "x"), VAR "base", ADD (VAR "x", CALL (VAR "f", VAR "x"))), CALL (VAR "f", CONST 5)))
let pgm8 = LET ("x", CONST 4, LETREC ("f", "x", IF (ISZERO (VAR "x"), CONST 0, ADD (VAR "x", CALL (VAR "f", SUB (VAR "x", CONST 1)))), CALL (VAR "f", VAR "x")))
let pgm9 = LET ("x", CONST 4, PROC ("x", VAR "x"))
let pgm10 = LET ("x", CONST 1, CALL (PROC ("x", ADD (VAR "x", CONST 10)), ADD (VAR "x", CONST 100)))
let pgm11 = LET ("x", CONST 1, LET ("base", CONST 4, LETREC ("f", "x", IF (ISZERO (VAR "x"), VAR "base", ADD (VAR "x", CALL (VAR "f", VAR "x"))), CALL (VAR "f", CONST 5))))

(* You can define datatypes and helper functions as necessary *)
let rec find : (var * ('a * 'b)list) -> exp
= fun (var, l) ->
  match l with
  | [] -> VAR var
  | hd::tl -> 
    (match hd with
    | (v, e) -> if(var = v) then e else find(var, tl))

let rec exist : (var * exp) -> int
= fun (var, exp) ->
  match exp with
  | CONST n -> 0
  | VAR x -> if(var = x) then 1 else 0
  | ADD (e1, e2) -> if((exist(var, e1) != 0) || (exist(var, e2) != 0)) then 1 else 0
  | SUB (e1, e2) -> if((exist(var, e1) != 0) || (exist(var, e2) != 0)) then 1 else 0
  | MUL (e1, e2) -> if((exist(var, e1) != 0) || (exist(var, e2) != 0)) then 1 else 0
  | DIV (e1, e2) -> if((exist(var, e1) != 0) || (exist(var, e2) != 0)) then 1 else 0
  | ISZERO e -> if(exist(var, e) != 0) then 1 else 0
  | READ -> 0
  | IF (e1, e2, e3) -> if((exist(var, e1) != 0) || (exist(var, e2) != 0) || (exist(var, e3) != 0)) then 1 else 0
  | LET (v, e1, e2) -> 
  	if(var = v) then 
  	  (if(exist(var, e1) = 1) then 1 else 0)
    else if (exist(var, e1) = 1 || exist(var, e2) = 1) then 1 
    else 0
  | LETREC(v1, v2, e1, e2) -> if(exist(var, e1) = 1 || exist(var, e2) = 1) then 1 else 0
  | PROC (v, e) -> if(var = v) then 1 else 0
  | CALL (e1, e2) -> if((exist(var, e1) != 0) || (exist(var, e2) != 0)) then 1 else 0

let rec convert : (exp * ('a * 'b)list) -> exp
= fun (exp, env) ->
  match exp with
  | CONST n -> CONST n
  | VAR x -> find (x, env)
  | ADD (e1, e2) -> 
    let x1 = convert (e1, env) in
    let x2 = convert (e2, env) in ADD (x1, x2)
  | SUB (e1, e2) -> 
    let x1 = convert (e1, env) in
    let x2 = convert (e2, env) in SUB (x1, x2)
  | MUL (e1, e2) -> 
    let x1 = convert (e1, env) in
    let x2 = convert (e2, env) in MUL (x1, x2)
  | DIV (e1, e2) -> 
    let x1 = convert (e1, env) in
    let x2 = convert (e2, env) in DIV (x1, x2)
  | ISZERO e -> ISZERO (convert (e, env))
  | READ -> READ
  | IF (e1, e2, e3) -> 
    let x1 = convert (e1, env) in
    let x2 = convert (e2, env) in
    let x3 = convert (e3, env) in IF (x1, x2, x3)
  | LET (v, e1, e2) -> 
  	if(exist(v, e2) = 1) then 
  		(let env = [(v, convert(e1, env))]@env in convert(e2, env))
   	else LET (v, e1, e2)
  | LETREC (v1, v2, e1, e2) -> LETREC (v1, v2, e1, e2)
  | PROC (v, e) -> 
    let env = env@[(v, e)] in
    let x = convert(e, env) in PROC (v, x)
  | CALL (e1, e2) -> 
    let x1 = convert (e1, env) in
    let x2 = convert (e2, env) in
    (match x1 with
    | VAR x -> 
      let x1 = find(x, env) in CALL (x1, x2)
    | _ -> CALL (x1, x2))


let rec expand : exp -> exp 
= fun exp -> convert (exp, [])
  




(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda

let pg1 = P ("a", V "a")
let pg2 = P ("a", P ("a", V "a"))
let pg3 = P ("a", P ("b", C (V "a", V "b")))
let pg4 = P ("a", C (V "a", P ("b", V "a")))
let pg5 = P ("a", V "b")
let pg6 = P ("a", C (V "a", P ("b", V "c")))
let pg7 = P ("a", P ("b", C (V "a", V "c")))

let rec doesExist : (var * 'a list) -> bool
= fun (var, env) ->
  match env with 
  | [] -> false
  | hd::tl -> if(var = hd) then true else doesExist(var, tl)

let rec isBound : (lambda * 'a list) -> bool
= fun (lam, env) ->
  match lam with
  | V v -> if(doesExist (v, env)) then true else false
  | P (v, l) -> isBound(l, env@[v])
  | C (l1, l2) -> isBound(l1, env) && isBound(l2, env)

let rec check : lambda -> bool
= fun lam -> 
  match lam with
  | V v -> false
  | P (v, l) -> isBound(l, [v])
  | C (l1, l2) -> false


