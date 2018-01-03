open Lang
open Util

type variable_map = (int,(id BatSet.t)) BatMap.t

let rec update_var_set xs env =
  match xs with
  |[] -> env
  |(id,_)::tl -> update_var_set tl (BatSet.add id env)

let rec var_exp env map exp =
  match exp with  
  | ELet (f, is_rec, xs, t, e1, e2) ->
    let env = if(is_rec) then BatSet.add f env else env in
    let env' = update_var_set xs env in
    let (_,map') = var_exp env' map e1 in
    var_exp (BatSet.add f env) map' e2
  | ECtor (c, es) ->  
    let (_,map') = var_exp_list es env map in
    (env,map')
  | EMatch (e, bs) ->
    let (_,map') = var_exp env map e in
    var_branch_list bs env map'
  | IF (e1,e2,e3) ->
    let (env',map') = var_exp env map e1 in
    let (env',map') = var_exp env map' e2 in
    var_exp env map' e3
  | Const _ 
  | EVar _ 
  | String _ -> (env,map)
  | EFun ((x, _), e) ->
    let env = BatSet.add x env in
    var_exp env map e
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
    let (env',map') = var_exp env map x1 in
    var_exp env map' x2
  | TRUE 
  | FALSE -> (env,map)
  | NOT x1 
  | MINUS x1 -> var_exp env map x1 
  | EList l1 
  | ETuple l1 -> var_exp_list l1 env map
  | Hole n ->
    (env,BatMap.add n env map)

and var_exp_list l env map=
  match l with
  |[] -> (env,map)
  |hd::tl ->
    let (_,map') = var_exp env map hd in
    var_exp_list tl env map' 

and var_branch_list bs env map=
  match bs with
  |[] -> (env,map)
  |(p,e)::tl ->
    let (env',map') = add_var_pat p env map in
    let (_,map') = var_exp env' map' e in
    var_branch_list tl env map'

and add_var_pat p env map=
  match p with
    | PInt _
    | PBool _ -> (env,map)
    | PList l
    | PTuple l 
    | PCtor(_, l) 
    | PCons l -> pat_list_env l env map
    | PVar x -> (BatSet.add x env,map)
    | PUnder -> (env,map)
    | Pats pl -> pat_list_env pl env map
    | PHole n ->
      (env,BatMap.add n env map)

and pat_list_env l env map =
  match l with
  |[] -> (env,map)
  |hd::tl -> 
    let (env,map) = add_var_pat hd env map in
    pat_list_env tl env map


let var_decl decl (env,map) =
  match decl with
  | DData _ -> (env,map)
  | DLet (x,is_rec,args,typ,exp) ->
    let (_,map) = var_exp env map (ELet (x,is_rec,args,typ,exp,EVar x)) in
    (BatSet.add x env,map)

let run prog = 
  let (_,var_table)= list_fold var_decl prog (BatSet.empty,BatMap.empty) in
  var_table
