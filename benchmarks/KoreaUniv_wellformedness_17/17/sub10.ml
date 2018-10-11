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
(* type let_con = var * exp * int *)
let rec search_con_list v l =
  match l with
  | hd::tl -> (
    match hd with
    | (var, e, count) -> if v = var then hd else search_con_list v tl
  )
  | [] -> (v, VAR v, (-1))

let rec add_let_con (v, e, c) l =
  match l with
  | hd::tl -> (
    match hd with
    | (v1, e1, c1) -> if v = v1 then (v, e, c)::tl else hd::(add_let_con (v, e, c) tl)
  )
  | [] -> [(v, e, c)]

let rec increase_count v l =
  match l with
  | hd::tl -> (
    match hd with
    | (var, e, count) -> if v = var then (var, e, count+1)::tl else increase_count v tl
  )
  | [] -> []

let let_con_list = ref [];;

let rec expand_rec : exp -> exp
= fun exp ->
  match exp with
  | CONST i -> CONST i
  | VAR v -> (
    let (_, e, _) = search_con_list v (!let_con_list) in
    let_con_list := increase_count v (!let_con_list);
    e
  )
  | ADD (e1, e2) -> ADD (expand_rec e1, expand_rec e2)
  | SUB (e1, e2) -> SUB (expand_rec e1, expand_rec e2)
  | MUL (e1, e2) -> MUL (expand_rec e1, expand_rec e2)
  | DIV (e1, e2) -> DIV (expand_rec e1, expand_rec e2)
  | ISZERO e -> ISZERO (expand_rec e)
  | READ -> READ
  | IF (e1, e2, e3) -> IF (expand_rec e1, expand_rec e2, expand_rec e3)
  | LET (v, e1, e2) -> (
    let new_e1 = expand_rec e1 in
    let_con_list := add_let_con (v, new_e1, 0) (!let_con_list);
    let new_e2 = expand_rec e2 in
    let (_, _, count) = search_con_list v (!let_con_list) in
    if count == 0 then LET (v, new_e1, new_e2) else new_e2
  )
  | LETREC (f, x, e1, e2) -> (
    LETREC (f, x, expand_rec e1, expand_rec e2)
    (*
    let new_e1 = expand_rec e1 in
    let_con_list := add_let_con (f, (PROC (x, new_e1)), 0) (!let_con_list);
    let new_e2 = expand_rec e2 in
    let (_, _, count) = search_con_list f (!let_con_list) in
    if count == 0 then LETREC (f, x, new_e1, new_e2) else new_e2
  *)
  )
  | PROC (v, e) -> PROC (v, expand_rec e)
  | CALL (e1, e2) -> CALL (expand_rec e1, expand_rec e2)


let expand : exp -> exp 
= fun exp -> 
  let_con_list := [];
  expand_rec exp


(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec search_var_list : var list -> var -> bool
= fun vl v ->
  match vl with
  | hd::tl -> if hd = v then true else search_var_list tl v
  | _ -> false

let rec check_rec : lambda -> var list -> bool
= fun lam vl ->
  match lam with
  | V v -> if search_var_list vl v then true else false
  | P (v, l) -> (
    let new_vl = v::vl in
    check_rec l new_vl
  )
  | C (l1, l2) -> (check_rec l1 vl) && (check_rec l2 vl)

let check : lambda -> bool
= fun lam -> 
  let var_list = [] in
  check_rec lam var_list
