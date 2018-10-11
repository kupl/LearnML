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
let pgm1 = LET ("x", CONST 1, VAR "x") (*CONST 1*)
let pgm2 = 
  LET ("f", PROC ("x", VAR "x"), 
    IF (CALL (VAR "f", ISZERO (CONST 0)), 
        CALL (VAR "f", CONST 11), 
        CALL (VAR "f", CONST 22)))
  (*
    IF (CALL (PROC ("x", VAR "x"), ISZERO (CONST 0)),
    CALL (PROC ("x", VAR "x"), CONST 11),
    CALL (PROC ("x", VAR "x"), CONST 22))
  *)
let pgm3 = LET ("x", ADD (CONST 1, ISZERO (CONST 0)), CONST 2)(* 결과 : LET ("x", ADD (CONST 1, ISZERO (CONST 0)), CONST 2)*)

(* You can define datatypes and helper functions as necessary *)

exception TypeError

type typ = TyInt | TyBool | TyFun of typ * typ | TyVar of tyvar |TyError
and tyvar = string
type typ_eqn = (typ * typ) list


(* type environment : var -> type *)
module TEnv = struct
  type t = var -> typ
  let empty = fun _ -> raise (Failure "Type Env is empty")
  let extend (x,t) tenv = fun y -> if x = y then t else (tenv y)
  let find tenv x = tenv x
end

let tyvar_num = ref 0

(* generate a fresh type variable *)
let fresh_tyvar () = (tyvar_num := !tyvar_num + 1; (TyVar ("t" ^ string_of_int !tyvar_num)))

let rec typerule : exp->TEnv.t->typ
= fun exp env -> match exp with 
  |CONST n -> TyInt
  | VAR x ->TEnv.find env x 
  | ADD (e1, e2)  | SUB (e1, e2)  | MUL (e1, e2) | DIV (e1, e2) -> (
                                                                    let ee1 = typerule e1 env in
                                                                    let ee2 = typerule e2 env in
                                                                    match ee1, ee2 with
                                                                    |TyInt, TyInt ->TyInt
                                                                    |_,_->TyError
                                                                  )
  | ISZERO e -> let ee = typerule e env in if ee=TyInt then TyBool else TyError
  | READ ->TyInt
  | IF(e1, e2, e3)->(
                      let ee1 = typerule e1 env in
                      let ee2 = typerule e2 env in
                      let ee3 = typerule e3 env in
                      match ee1 with
                      |TyBool->if ee2=ee3 then ee3 else TyError
                      |_->TyError)
  | LET(x, e1, e2) ->(
                      let ee1 = typerule e1 env in
                      let ee2 = typerule e2 (TEnv.extend (x, ee1) env) in
                      ee2
                        )
  | LETREC (f, x, e1, e2) ->(
                        let newt1 = fresh_tyvar() in
                        let newt2 = fresh_tyvar() in
                        let env1 = TEnv.extend(x, newt2) env in
                        let env2 = TEnv.extend(f, TyFun(newt2, newt1)) env1 in
                        let env3 = TEnv.extend(f, TyFun(newt2, newt1)) env in
                        let ee1 = typerule e1 env2 in
                        let ee2 = typerule e2 env3 in
                        if ee1 = newt1 then ee2 else TyError
                      )
  | PROC(x, e) ->(  let newt = fresh_tyvar() in
                    let ee = typerule e (TEnv.extend (x, newt) env) in 
                    TyFun(newt, ee)
                  )
  | CALL(e1, e2) ->(
                    let ee1 = typerule e1 env in
                    let ee2 = typerule e2 env in
                    match ee1 with
                    |TyFun(t1, t2)->if t1=ee2 then t2 else TyError
                    |_->TyError
                  )

let gg = PROC("x", ADD(VAR "y", CONST 1))


let rec change x exp1 exp2 
=match exp2 with
|CONST n -> CONST n
|VAR y -> if x=y then exp1 else exp2
|ADD(e1, e2) -> (let che1 = change x exp1 e1 in
                let che2 = change x exp1 e2 in
                ADD(che1, che2)
              )
|SUB(e1, e2)-> (let che1 = change x exp1 e1 in
                let che2 = change x exp1 e2 in
                SUB(che1, che2)
              )
|MUL(e1, e2)-> (let che1 = change x exp1 e1 in
                let che2 = change x exp1 e2 in
                MUL(che1, che2)
              )
|DIV(e1, e2)-> (let che1 = change x exp1 e1 in
                let che2 = change x exp1 e2 in
                DIV(che1, che2)
              )
|ISZERO e -> let che = change x exp1 e in ISZERO che
|READ -> READ
|IF(e1, e2, e3)->(let che1 = change x exp1 e1 in 
                  let che2 = change x exp1 e2 in
                  let che3 = change x exp1 e3 in
                  IF(che1, che2, che3)
                  )
|LET(y, e1, e2) -> (
                      let che1 = change x exp1 e1 in
                      let che2 = change x exp1 e2 in
                      LET(y, che1, che2)
                    )
|LETREC (f, x, e1, e2) -> ( 
                            let che1 = change x exp1 e1 in
                            let che2 = change x exp1 e2 in
                            LETREC(f, x, che1, che2)

                          )
| PROC(y, e) -> (let che = change x exp1 e in
                  PROC(y, che)
                )
| CALL(e1, e2) ->(let che1 = change x exp1 e1 in
                  let che2 = change x exp1 e2 in
                  CALL(che1, che2))


let rec expand : exp -> exp 
= fun exp -> match exp with
|LET (x, e1, e2) -> (let tye1 = (typerule e1 TEnv.empty) in
                        match tye1 with
                        |TyInt | TyBool | TyFun (_,_) |TyVar _  -> expand(change x e1 e2)
                        |_ -> exp      
                    )
|_->exp

            
let xx = PROC("x",VAR "x")

let a = ADD (CONST 1, ISZERO (CONST 0))
let pgm5 = IF (CALL (PROC ("x", VAR "x"), ISZERO (CONST 0)),CALL (PROC ("x", VAR "x"), CONST 11),CALL (PROC ("x", VAR "x"), CONST 22))

let pgm6 = LET("f", PROC("x", VAR "x"), LET("g", PROC("x", ADD(VAR "x", CONST 1)),ADD(CALL(VAR "f", CONST 1), CALL(VAR "g", CONST 2))))

(* LET x e1 e2 에서 e1이 safe한지 체크, safe하지 않으면 바꾸지 않음. safe하면 바꿈
*)

(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda


let rec remove n ls 
= match ls with
|[]->[]
|hd::tl -> if hd=n then remove n tl else hd::(remove n tl)

let rec length ls
=match ls with
|[] ->0
|hd::tl -> 1+(length tl)

let rec union l1 l2 
=match l1 with
|[] -> l2
|hd::tl -> union tl (hd::l2)

let rec setfree lam
= match lam with
|V x -> [x]
|P(x, lam1) -> remove x (setfree lam1)
|C(lam1, lam2) -> union (setfree lam1) (setfree lam2)

let rec check : lambda -> bool
= fun lam -> 
let s = setfree lam in
(if length s = 0 then true else false)


(*
let rec ff a = 
  let s = setfree a in s
*)

(*true*)
let lam1 = P ("a", V "a")
let lam2 = P ("a", P ("a", V "a"))
let lam3 = P ("a", P ("b", C (V "a", V "b")))
let lam4 = P ("a", C (V "a", P ("b", V "a")))


(*false*)
let lam9 = P("b", C(V "a", V "a"))
let lam5 = P ("a", V "b")
let lam6 = P ("a", C (V "a", P ("b", V "c")))
let lam7 = P ("a", P ("b", C (V "a", V "c")))



(*

(*free인지를 알려주는 것*)
let rec existfree : lambda-> var ->bool 
= fun l x -> match l with
|V y -> if y=x then false else true
|P(y, lam) -> if x=y then (existfree lam x) else (existfree lam x)||(existfree lam y)
|C(lam1, lam2) -> (
                    let lam11 = existfree lam1 x in
                    let lam22 = existfree lam2 x in
                    match lam11, lam22 with
                    |true, true->true
                    |true, false ->true
                    |false, true->true
                    |false, false ->false
                  )

(*
- An occurence of var x is said to be bound when it occurs inside λx., otherwise said to be free
- checks if a given program is well-formed. A program is said to be well-
  formed if and only if the program does not contain free variables; i.e., every
  variable name is bound by some procedure that encompasses the variable.
*)
let rec check : lambda -> bool
= fun lam -> match lam with
|V x -> false
|P(x, lam) -> ((*lam안에 y가 있으면 bound*)
                not(existfree lam x)
              )
|C(lam1, lam2) ->(
                  let l1 = check lam1 in
                  let l2 = check lam2 in
                  match l1, l2 with
                  |true,true -> true
                  |_, _-> false
                 )


let rec bound x lam
=match lam with
|V y -> if y=x then true else false
|P(y, lam1) ->  if (x=y) then bound x lam1
                else if (x!=y) then (bound x lam1)&&(bound y lam1)
                else if(bound x lam1) then true 
                else false
|C(lam1, lam2) ->  if(bound x lam1)||(bound x lam2) then true
                  else false
*)