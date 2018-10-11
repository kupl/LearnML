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
type value = 
    Int of int
  | Bool of bool
  | Closure of var * exp * env
  | RecClosure of var * var * exp * env
  | Loc of loc
and loc = int
and env = (var * value) list
and mem = (loc * value) list

let empty_env = []
let extend_env (x, v) e = (x, v)::e
let rec apply_env e x = 
  match e with 
  | [] -> raise (Failure (x ^ " is unbound in Env"))
  | (y, v)::tl -> if x = y then v else apply_env tl x

let empty_mem = []
let extend_mem (l,v) m = (l,v)::m
let rec apply_mem m l = 
  match m with
  | [] -> raise (Failure ("Location " ^ string_of_int l ^ " is unbound in mem"))
  | (y, v)::tl -> if l = y then v else apply_mem tl l

let counter = ref 0
let new_location () = counter:=!counter+1;!counter

exception UndefinedSemantics

let rec eval : exp -> env -> mem -> value * mem * bool
= fun exp env mem -> match exp with
  | CONST n -> (Int n, mem, true)
  | VAR x -> ((apply_env env x), mem, true)
  | ADD (e1, e2) -> let (n1, m1, t1) = (eval e1 env mem) in
                    let (n2, m2, t2) = (eval e2 env m1) in
                    (match t1, t2, n1, n2 with
                      | true, true, Int n1, Int n2 -> (Int ((+) n1 n2), m2, true)
                      | _ -> (Int 1, mem, false))
  | SUB (e1, e2) -> let (n1, m1, t1) = (eval e1 env mem) in
                    let (n2, m2, t2) = (eval e2 env m1) in
                    (match t1, t2, n1, n2 with
                      | true, true, Int n1, Int n2 -> (Int ((-) n1 n2), m2, true)
                      | _ -> (Int 1, mem, false))
  | MUL (e1, e2) -> let (n1, m1, t1) = (eval e1 env mem) in
                    let (n2, m2, t2) = (eval e2 env m1) in
                    (match t1, t2, n1, n2 with
                      | true, true, Int n1, Int n2 -> (Int (( * ) n1 n2), m2, true)
                      | _ -> (Int 1, mem, false))
  | DIV (e1, e2) -> let (n1, m1, t1) = (eval e1 env mem) in
                    let (n2, m2, t2) = (eval e2 env m1) in
                    (match t1, t2, n1, n2 with
                      | true, true, Int n1, Int n2 -> if(n2 = 0) then (Int 1, m2, false) else (Int ((/) n1 n2), m2, true)
                      | _ -> (Int 1, mem, false))
  | ISZERO e -> (match (eval e env mem) with
                  | (Int n, m1, true) -> if (n = 0) then (Bool true, m1, true) else (Bool false, m1, true)
                  | _ -> (Int 1, mem, false))
  | READ -> (Int (read_int()), mem, true)
  | IF (e1, e2, e3) -> (match (eval e1 env mem) with
                  | (Bool true, m1, true) -> (eval e2 env m1)
                  | (Bool false, m1, true) -> (eval e3 env m1)
                  | _ -> (Int 1, mem, false))
  | LET (x, e1, e2) -> let (v1, m1, t) = (eval e1 env mem) in
                       let env1 = (extend_env (x, v1) env) in
                       if t = true then (eval e2 env1 m1) else (Int 1, mem, false)
  | LETREC (f, x, e1, e2) -> let env1 = (extend_env (f, RecClosure (f, x, e1, env)) env) in
                             (eval e2 env1 mem)
  | PROC (x, e) -> (Closure (x, e, env), mem, true)
  | CALL (e1, e2) -> (match (eval e1 env mem) with
                      | (Closure (x, e, env1), m1, true) -> 
                      let (v, m2, t) = (eval e2 env m1) in
                      let env2 = (extend_env (x, v) env1) in
                      if (t = true) then (eval e env2 m2) else (Int 1, mem, false)
                      | (RecClosure (f, x, e, env1), m1, true) ->
                      let (v, m2, t) = (eval e2 env m1) in
                      let env2 = (extend_env (x, v) env1) in
                      let env3 = (extend_env (f, RecClosure (f, x, e, env1)) env2) in
                      if (t = true) then (eval e env3 m2) else (Int 1, mem, false)
                      | _ -> (Int 1, mem, false))

let rec is_var
= fun v exp2 -> match exp2 with
  | CONST n -> false
  | VAR x -> if (x = v) then true else false
  | ADD(e1, e2) -> (is_var v e1)||(is_var v e2)
  | SUB(e1, e2) -> (is_var v e1)||(is_var v e2)
  | MUL(e1, e2) -> (is_var v e1)||(is_var v e2)
  | DIV(e1, e2) -> (is_var v e1)||(is_var v e2)
  | READ -> false
  | ISZERO e -> is_var v e
  | IF(e1, e2, e3) -> (is_var v e1)||(is_var v e2)||(is_var v e3)
  | LET(x, e1, e2) -> (is_var v e1)||(is_var v e2)
  | LETREC(f, x, e1, e2) -> (is_var v e1)||(is_var v e2)
  | PROC(x, e1) -> (is_var v e1)
  | CALL(e1, e2) -> (is_var v e1)||(is_var v e2)
  
let rec find_var 
= fun v exp1 exp2 -> match exp2 with
  | CONST n -> CONST n
  | VAR x -> if (x = v) then exp1 else VAR x
  | ADD (e1, e2) -> ADD(find_var v exp1 e1, find_var v exp1 e2)
  | SUB (e1, e2) -> SUB(find_var v exp1 e1, find_var v exp1 e2)
  | MUL (e1, e2) -> MUL(find_var v exp1 e1, find_var v exp1 e2)
  | DIV (e1, e2) -> DIV(find_var v exp1 e1, find_var v exp1 e2)
  | READ -> READ (*must edit*)
  | ISZERO e -> ISZERO (find_var v exp1 e)
  | IF (e1, e2, e3) -> IF(find_var v exp1 e1, find_var v exp1 e2, find_var v exp1 e3)
  | LET (x, e1, e2) ->  LET(x, find_var v exp1 e1, find_var v exp1 e2)
  | LETREC (f, x, e1, e2) -> if (x = v) then LETREC (f, x, e1, find_var v exp1 e2) else LETREC (f, x, find_var v exp1 e1, find_var v exp1 e2)
  | PROC (x, e1) -> if (x = v) then PROC (x, e1) else PROC (x, find_var v exp1 e1)
  | CALL (e1, e2) -> CALL (find_var v exp1 e1, find_var v exp1 e2)
                    
let rec expand : exp -> exp 
= fun exp -> (*match eval exp empty_env empty_mem with
  | (_, _, _) ->*) (match exp with
      | CONST n -> CONST n
      | VAR x -> VAR x
      | ADD (e1, e2) -> ADD (e1, e2)
      | SUB (e1, e2) -> SUB (e1, e2)
      | MUL (e1, e2) -> MUL (e1, e2)
      | DIV (e1, e2) -> DIV (e1, e2)
      | ISZERO e -> ISZERO e
      | READ -> READ
      | IF (e1, e2, e3) -> IF (e1, e2, e3)
      | LET (x, e1, e2) -> (match (is_var x e2) with
          | true ->  (match e2 with
                       | LET (y, e3, e4) -> (match (is_var x e3) with
                          |true -> let e5 = find_var x e1 e3 in
                                            if (y = x) then find_var y e5 e4
                                                
                                            (*(find_var y e5 e4)*)
                                            else if (is_var x e4) then  let e6 = find_var x e1 e4 in
                                                (* find_var y e5 e6*)
                                                (expand (LET(y, e5, e6)))
                                            else (expand (LET(y, e5, e4)))
                          |false -> let e5 = find_var x e1 e3 in
                                    if (y = x) then let e7 = find_var y e5 e4 in
                                                (expand (LET(x, e5, e7)))
                                    else if (is_var x e4) then let e6 = find_var x e1 e4 in
                                    (expand (LET(y, e5, e6)))                                             else (expand (LET(y, e5, e4))))

                       (*| LET (y, e3, e4) -> find_var y e3 e4*)
                       | _ -> find_var x e1 e2)
          | false -> LET(x, e1, e2))
      | LETREC (f, x, e1, e2) ->  LETREC (f, x, e1, e2)
      | PROC (x, e) -> PROC (x, e)
      | CALL (e1, e2) -> CALL (e1, e2))
 (* | (_, _, false) -> exp *)

(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
(*and var = string*)


let rec is_there
= fun x arr -> match arr with
  | [] -> false
  | hd::tl -> if x = hd then true else (is_there x tl)

let rec find
= fun lam arr -> match lam with
  | V x -> (is_there x arr, arr)
  (*(match arr with
      | [] -> (false, [])
      | hd::tl -> if x = hd then (true, arr) else (find (V x) tl))*)
  | P (x, l) -> let arr1 = x::arr in 
                (match l with
                | V x1 -> find (V x1) arr1
                | P (x1, l1) -> find (P (x1, l1)) arr1
                | C (l1, l2) -> (match l1 with
                    | V x2 -> let (t1, a1) = (find (V x2) arr1) in
                              let (t2, a2) = (find l2 a1) in
                              (t1&&t2, a2)
                    | _ -> find l arr1))
  | C (l1, l2) -> let (t1, a1) = (find l1 arr) in
                  let (t2, a2) = (find l2 a1) in
                  (t1&&t2, a2)
(*(find l1 arr)&&(find l2 arr)*)

let rec check : lambda -> bool
= fun lam -> match (find lam []) with
  | (true, _) -> true
  | (false, _) -> false
