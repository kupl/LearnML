type program = exp
and exp = 
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

exception TypeError

type typ = TyInt | TyBool | TyFun of typ * typ | TyVar of tyvar
and tyvar = string
type typ_eqn = (typ * typ) list

(* type environment : var -> type *)
type tenv = var -> typ
let tenv_empty = fun _ -> raise (Failure "Type Env is empty")
let tenv_extend (x,t) tenv = fun y -> if x = y then t else (tenv y)
let tenv_find tenv x = tenv x

(* substitution *)
type subst = (tyvar * typ) list
let subst_empty = []
let subst_find x subst = List.assoc x subst

(* walk through the type, replacing each type variable by its binding in the substitution *)
let rec subst_apply : typ -> subst -> typ
=fun typ subst ->
  match typ with
  | TyInt -> TyInt
  | TyBool -> TyBool 
  | TyFun (t1,t2) -> TyFun (subst_apply t1 subst, subst_apply t2 subst)
  | TyVar x -> if List.exists (fun (y, typ) -> x = y) subst then subst_find x subst else typ

(* add a binding (tv,ty) to the subsutition and propagate the information *)
let subst_extend tv ty subst = 
  (tv,ty) :: (List.map (fun (x,t) -> (x, subst_apply t [(tv,ty)])) subst)

let tyvar_num = ref 0

let fresh_tyvar _ = 
  let _ = tyvar_num := !tyvar_num + 1 in
  TyVar ("t" ^ string_of_int !tyvar_num)


exception Testing;;
let rec gen_equations : tenv -> exp -> typ -> typ_eqn 
=fun tenv e ty -> (* HW1: TODO *)
  match e with
    | CONST(n) -> [(ty,TyInt)]
    | VAR(var) -> let a =  tenv_find tenv var in
                   [(ty,a)]
    | ADD(exp1,exp2) -> let a = (ty,TyInt) in
                let new_e1 = fresh_tyvar () in
                let a = a::[(new_e1,TyInt)] in
                let b = gen_equations tenv exp1 new_e1 in
                let a = List.append a b in
                let new_e2 = fresh_tyvar () in
                let a = List.append a [(new_e2,TyInt)] in
                let c = gen_equations tenv exp2 new_e2 in
                          List.append a c  
                          
    | SUB(exp1,exp2) -> let new_e1 = fresh_tyvar () in
              let eqn = gen_equations tenv exp1 new_e1 in
              let eqn = (new_e1,TyInt)::eqn in
              let new_e2 = fresh_tyvar () in
              let a = gen_equations tenv exp2 new_e2 in
              let a = (new_e2,TyInt)::a in
              let eqn = (ty,TyInt)::eqn in
              List.append eqn a
              
    | MUL(exp1,exp2) -> let new_e1 = fresh_tyvar () in
              let eqn = gen_equations tenv exp1 new_e1 in
              let eqn = (new_e1,TyInt)::eqn in
              let new_e2 = fresh_tyvar () in
              let a = gen_equations tenv exp2 new_e2 in
              let a = (new_e2,TyInt)::a in
              let eqn = (ty,TyInt)::eqn in
              List.append eqn a
    
    | DIV(exp1,exp2) -> let new_e1 = fresh_tyvar () in
              let eqn = gen_equations tenv exp1 new_e1 in
              let eqn = (new_e1,TyInt)::eqn in
              let new_e2 = fresh_tyvar () in
              let a = gen_equations tenv exp2 new_e2 in
              let a = (new_e2,TyInt)::a in
              let eqn = (ty,TyInt)::eqn in
              List.append eqn a
    
    | ISZERO(exp) -> let eqn = (ty,TyBool) in
             let new_e = fresh_tyvar () in
             let a = gen_equations tenv exp new_e in
             let a = (new_e,TyInt)::a in
             eqn::a
    
    | READ -> [(ty,TyInt)]
    
    | IF (exp1, exp2, exp3) -> let new_e1 = fresh_tyvar () in
                   let b = gen_equations tenv exp1 new_e1 in
                   let new_e2 = fresh_tyvar () in
                   let eqn = (ty, new_e2)::(new_e1,TyBool)::b in
                   let b = gen_equations tenv exp2 new_e2 in
                   let eqn = List.append eqn b in
                   let new_e3 = fresh_tyvar () in
                   let eqn = (ty, new_e3)::eqn in
                   let b = gen_equations tenv exp3 new_e3 in
                   List.append eqn b
    
    | LET(var,exp1,exp2) -> let new_e1 = fresh_tyvar () in
                            let a = gen_equations tenv exp1 new_e1 in
                            let b = (match a with
                                     (new_e1,y)::tl -> tenv_extend(var, y) tenv 
                                      |_ -> raise TypeError) in
                           let new_e2 = fresh_tyvar () in
                           let c = gen_equations b exp2 new_e2 in
                           let a = List.append a c in
                                    (match c with
                                       (_,typ)::tl -> (ty,typ)::a
                                       |_ -> raise TypeError) 
                     
    | LETREC(var1,var2,exp1,exp2) -> let func_v1 = fresh_tyvar () in
                                     let new_rt = fresh_tyvar () in
                                     let new_arg = fresh_tyvar () in
                                     let a = tenv_extend(var1,TyFun(new_arg,new_rt)) tenv in
                                     let a = tenv_extend(var2,new_arg) a in
                                     let eqn = gen_equations a (PROC(var2,exp1)) func_v1 in
                                     let f = List.assoc (func_v1) eqn in
                                     let b = tenv_extend(var1,f) a in
                                     (match f with
                                       TyFun(x,y)->let eqn = (new_rt,y)::eqn in
                                                   let new_e2 = fresh_tyvar () in
                                                   let a = gen_equations a exp2 new_e2 in
                                                   let t3 = List.assoc new_e2 a in
                                                   let eqn = (ty,t3)::eqn in
                                                   List.append eqn a
                                       |_ -> raise TypeError)
                                      
                
    | PROC(var,exp) -> 
               let new_arg = fresh_tyvar () in
               let tenv = tenv_extend (var,new_arg) tenv in
               let new_e = fresh_tyvar () in
               let eqn = gen_equations tenv exp new_e in
               (ty,TyFun(new_arg,new_e))::eqn
               
    | CALL(exp1,exp2) -> (match exp1 with
                          VAR(v) -> let f = tenv_find tenv v in 
                                   let new_e1 = fresh_tyvar () in
                                   let new_e2 = fresh_tyvar () in
                                   let a = gen_equations tenv exp2 new_e2 in
                                   let eqn = (new_e1,f)::a in
                                   let b = List.assoc new_e2 eqn in
                                   (match f with
                                        TyFun(c,d) -> let eqn = (c,b)::eqn in
                                                       (ty,d)::eqn
                                        |TyVar x -> let new_arg = fresh_tyvar () in
                                                    let new_rt = fresh_tyvar () in
                                                    let eqn = (f, TyFun(new_arg,new_rt))::eqn in
                                                    let eqn = (new_arg, b)::eqn in
                                                    (ty,new_rt)::eqn
                                        
                                        |_ -> raise TypeError)
                          |_ -> let new_e1 = fresh_tyvar () in
                                       let new_e2 = fresh_tyvar () in
                                       let eqn = gen_equations tenv exp1 new_e1 in
                                       let a = gen_equations tenv exp2 new_e2 in
                                       let f = List.assoc new_e1 eqn in
                                       let b = List.assoc new_e2 a in
                                       (match f with
                                         TyFun(c,d) -> let eqn = (ty,d)::(c,b)::eqn in
                                                       List.append eqn a
                                         |TyVar d -> let new_arg = fresh_tyvar () in
                                                    let new_rt = fresh_tyvar () in
                                                    let eqn = (f, TyFun(new_arg,new_rt))::eqn in
                                                    let eqn = (new_arg, b)::eqn in
                                                    (ty,new_rt)::eqn
                                          |_ -> raise TypeError)) 
             
    | _ -> [];;

exception NotInt;;
exception SameOccur;;


let rec b : subst -> subst
=fun subst -> match subst with
 (v, t)::tl -> (match t with
                       TyFun(a,c) -> if (a = TyVar v) || (c = TyVar v) then raise TypeError
                                     else 
                                       let h = b ((v,a)::(v,c)::tl) in
                                       subst
                                           
                       |_ -> (v,t)::(b tl))
 |[] -> subst;;



let rec a : typ_eqn -> subst -> subst
=fun eqn subst -> match eqn with
  |(TyVar v,t2)::tl -> (match subst with
              |[] -> let subst = [(v,t2)] in
                     a tl subst
              |h::l -> let t2 = subst_apply t2 subst in 
                       let t1 = (if List.exists (fun (y, ty) -> v = y) subst then 
                                 subst_find v subst 
                                 else t2) in
                         (if t1 = t2 then let subst = subst_extend v t2 subst in
                                         (match t2 with
                                           |TyVar u -> (let subst = subst_extend u (TyVar v) subst 
                                                               in a tl subst)
                                           |_ -> a tl subst)

                        else 
                          (match t2 with 
                             TyVar x -> (match t1 with
                                          |TyVar u -> let subst = subst_extend v t2 subst in
                                                      let subst = subst_extend x t1 subst in
                                                      let subst = subst_extend u t2 subst in
                                                      a tl subst
                                          |_ -> let subst = subst_extend v t2 subst in
                                                let subst = subst_extend x t1 subst in
                                                a tl subst)
                             
                             |TyFun(x,y) -> (match t1 with 
                                             TyFun(d,f) -> a ((x,d)::(y,f)::tl) subst
                                             
                                             |TyVar d -> let subst = subst_extend v t2 subst in
                                                         let subst = subst_extend d t2 subst in
                                                         a tl subst
                                            
                                             |_ -> raise TypeError)
                             
                             |TyInt -> (match t1 with
                                                  TyVar y -> (let subst = subst_extend v t2 subst in
                                                                let subst = subst_extend y t2 subst in
                                                                a tl subst)
                                                  |_ -> raise TypeError)
                             |TyBool -> (match t1 with
                                                  TyVar y -> (let subst = subst_extend v t2 subst in
                                                                let subst = subst_extend y t2 subst in
                                                                a tl subst)
                                                  |_ ->  raise TypeError))))

  |(TyBool,t2)::tl -> (match t2 with
                     TyVar v -> a ((t2, TyBool)::tl) subst
                     |TyBool -> a tl subst
                     |_ -> raise TypeError)
  
  |(TyInt,t2)::tl -> (match t2 with
                     TyVar v -> a ((t2, TyInt)::tl) subst
                     |TyInt -> a tl subst
                     |_ -> raise TypeError)

  |(TyFun(t,b),t2)::tl -> (match t2 with
                        TyFun(c,d) -> a ((t,c)::(b,d)::tl) subst
                        |TyVar v -> a ((t2,TyFun(t,b))::tl) subst
                        |_ -> raise TypeError)                                
  |[] -> b subst;;


let solve : typ_eqn -> subst
=fun eqns -> match eqns with
               |hd::tl -> let ans = a eqns subst_empty in
               List.rev ans
               |[] -> subst_empty;;
    
let typeof : exp -> typ 
= fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations tenv_empty exp new_tv in  
  let subst = solve eqns in
  let ty = subst_apply new_tv subst in
  ty