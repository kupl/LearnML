open Lang
open Util

type hole_var_map = (int,(id BatSet.t)) BatMap.t

let var_table = ref BatMap.empty

let rec update_var_set xs env =
  match xs with
  |[] -> env
  |(id,_)::tl -> update_var_set tl (BatSet.add id env)

let rec var_exp env exp =
  match exp with  
  | ELet (f, is_rec, xs, t, e1, e2) ->
    let env = if(is_rec) then BatSet.add f env else env in
    let new_env = update_var_set xs env in
    let new_env = var_exp new_env e1 in
    var_exp (BatSet.add f env) e2
  | ECtor (c, es) ->  
    var_exp_list es env 
  | EMatch (e, bs) ->
      let env = var_exp env e in
      var_branch_list bs env
  | IF (e1,e2,e3) ->
    let env = var_exp env e1 in
    let env = var_exp env e2 in
    var_exp env e3
  | Const n -> env
  | EVar x -> env
  | String id -> env
  | EFun ((x, _), e) ->
    let env = BatSet.add x env in
    var_exp env e
  | ADD (x1,x2) 
  | SUB (x1,x2)  
  | MUL (x1,x2)  
  | DIV (x1,x2)  
  | MOD (x1,x2)  
  | OR (x1,x2)  
  | AND (x1,x2)  
  | LESS (x1,x2)  
  | LARGER (x1,x2)  
  | EQUAL (x1,x2)  
  | NOTEQ (x1,x2)  
  | LESSEQ (x1,x2)  
  | LARGEREQ (x1,x2)  
  | AT (x1,x2) 
  | DOUBLECOLON (x1,x2)
  | EApp (x1, x2) ->
    let env = var_exp env x1 in
    var_exp env x2
  | TRUE -> env
  | FALSE -> env
  | NOT x1 
  | MINUS x1 -> var_exp env x1 
  | EList (l1) 
  | ETuple (l1) -> var_exp_list l1 env
  | Hole n ->
    let _ = var_table:= BatMap.add n env (!var_table) in
    env

and var_exp_list l env=
  match l with
  |[] -> env
  |hd::tl ->
    let env = var_exp env hd in
    var_exp_list tl env  

and var_branch_list bs env =
  match bs with
  |[] -> env
  |(p,e)::tl ->
    let new_env = add_var_pat p env in
    let new_env = var_exp new_env e in
    var_branch_list tl env

and add_var_pat p env =
  match p with
    | PInt n -> env
    | PBool b -> env
    | PList l-> pat_list_env l env
    | PTuple l -> pat_list_env l env
    | PCtor(x, l) -> pat_list_env l env
    | PCons l -> pat_list_env l env
    | PVar x -> BatSet.add x env
    | PUnder -> env
    | Pats pl -> pat_list_env pl env
    | PHole n ->
      let _ = var_table:= BatMap.add n env (!var_table) in
      env

and pat_list_env l env =
  match l with
  |[] -> env
  |hd::tl -> 
    let env = add_var_pat hd env in
    pat_list_env tl env


let var_decl decl env =
  match decl with
  | DData _ -> env
  | DLet (x,is_rec,args,typ,exp) ->
    let _ = var_exp env (ELet (x,is_rec,args,typ,exp,EVar x)) in
    if(is_rec) then BatSet.add x env else env

let run : prog -> unit
=fun decls ->
  let _ = var_table := BatMap.empty in
  let _ = list_fold var_decl decls BatSet.empty in
  (*let _ = print_endline("----------------") in
  let _ = Print.print_pgm decls in
          let _ = BatMap.iter(
            fun n set ->
            let _ = print_endline(string_of_int(n)) in
            let _ = BatSet.iter(
              fun id -> print_endline(id)
            ) set in 
            print_endline("-----")
          ) (!var_table)
          in
  let _ = print_endline("-----------------") in
*)  ()
