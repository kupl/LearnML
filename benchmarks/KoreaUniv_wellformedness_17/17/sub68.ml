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

let rec checker : exp -> var -> bool
= fun exp a ->
  (match exp with
   | CONST n -> false
   | VAR x -> if x=a then true else false
   | ADD (e1, e2) -> (checker e1 a) || (checker e2 a)
   | SUB (e1, e2) -> (checker e1 a) || (checker e2 a)
   | MUL (e1, e2) -> (checker e1 a) || (checker e2 a)
   | DIV (e1, e2) -> (checker e1 a) || (checker e2 a)
   | ISZERO e -> checker e a
   | READ -> false
   | IF (e1, e2, e3) -> (checker e1 a) || (checker e2 a) || (checker e3 a)
   | LET (x, e1, e2) -> (x=a) || (checker e1 a) || (checker e2 a)
   | LETREC (x1, x2, e1, e2) -> (x1=a) || (x2=a) || (checker e1 a) || (checker e2 a)
   | PROC (x, e) -> (x=a) || (checker e a)
   | CALL (e1, e2) -> (checker e1 a) || (checker e2 a))
  
let rec substitute : var -> exp -> exp -> exp
= fun x e1 e2 ->
  (match e2 with
   | CONST n -> CONST n
   | VAR v -> if (x=v) then e1 else VAR v
   | ADD (e3, e4) -> ADD (substitute x e1 e3, substitute x e1 e4)
   | SUB (e3, e4) -> SUB (substitute x e1 e3, substitute x e1 e4) 
   | MUL (e3, e4) -> MUL (substitute x e1 e3, substitute x e1 e4)
   | DIV (e3, e4) -> DIV (substitute x e1 e3, substitute x e1 e4)
   | ISZERO e -> ISZERO (substitute x e1 e)
   | READ -> READ
   | IF (e3, e4, e5) -> IF (substitute x e1 e3, substitute x e1 e4, substitute x e1 e5)
   | LET (v, e3, e4) -> LET (v, substitute x e1 e3, substitute x e1 e4)
   | LETREC (v1, v2, e3, e4) -> LETREC (v1, v2, substitute x e1 e3, substitute x e1 e4)
   | PROC (v, e) -> PROC (v, substitute x e1 e)
   | CALL (e3, e4) -> CALL (substitute x e1 e3, substitute x e1 e4))


(* You can define datatypes and helper functions as necessary *)
let rec expand : exp -> exp 
= fun exp ->
  (match exp with
   | CONST n -> CONST n
   | VAR x -> VAR x
   | ADD (e1, e2) -> ADD (expand e1, expand e2)
   | SUB (e1, e2) -> SUB (expand e1, expand e2)
   | MUL (e1, e2) -> MUL (expand e1, expand e2)
   | DIV (e1, e2) -> DIV (expand e1, expand e2)
   | ISZERO e -> ISZERO (expand e)
   | READ -> READ
   | IF (e1, e2, e3) -> IF (expand e1, expand e2, expand e3)
   | LET (x, e1, e2) ->
     (if (checker e2 x == true) then substitute x (expand e1) e2
     else LET (x, expand e1, expand e2))
   | LETREC (x1, x2, e1, e2) -> LETREC (x1, x2, expand e1, expand e2)
   | PROC (x, e) -> PROC (x, expand e)
   | CALL (e1, e2) -> CALL (expand e1, expand e2))

(**********************)
(*   Problem 2        *)
(**********************)
type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

(*test cases*)
let test1 = P ("a", V "a")
let test2 = P ("a", P ("a", V "a"))
let test3 = P ("a", P ("b", C (V "a", V "b")))
let test4 = P ("a", C (V "a", P ("b", V "a")))
let test5 = P ("a", V "b")
let test6 = P ("a", C (V "a", P ("b", V "c")))
let test7 = P ("a", P ("b", C (V "a", V "c")))


let rec erase : var -> var list -> var list
= fun v lst ->
  (match lst with
   | [] -> []
   | hd::tl -> if (v=hd) then (erase v tl) else hd::(erase v tl))

let rec check_help : lambda -> var list
= fun lam ->
  (let lst = [] in
   match lam with
   | V v -> v::lst
   | P (v, l) -> erase v (check_help l)
   | C (l1, l2) -> (check_help l1)@(check_help l2))

let rec check : lambda -> bool
= fun lam ->
  (match lam with
   | V v -> false
   | P (v, l) -> if List.length (erase v (check_help l)) == 0 then true else false
   | C (l1, l2) -> false)

