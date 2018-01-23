open Lang
open Util


(* Control variables *)
let count = ref 0
let infinite_count = ref 0
let start_time = ref 0.0

(* Block task *)
let rec is_fun : value -> bool
= fun v ->
  match v with
  | VList vs -> List.exists is_fun vs 
  | VTuple vs -> List.exists is_fun vs
  | VCtor (x, vs) -> List.exists is_fun vs
  | VFun (x, e, closure) -> true
  | VFunRec (f, x, e, closure) -> true
  | VBlock (f, vs) -> true
  | _ -> false

let rec update_closure : env -> value -> value
= fun env v ->
  match v with
  | VList vs -> VList (List.map (update_closure env) vs)
  | VTuple vs -> VTuple (List.map (update_closure env) vs)
  | VCtor (x, vs) -> VCtor (x, List.map (update_closure env) vs)
  | VFun (x, e, closure) -> VFun (x, e, env)
  | VFunRec (f, x, e, closure) -> VFunRec (f, x, e, env)
  | _ -> v

let rec find_callee : id -> (id * value) list -> value
= fun x vs ->
  match vs with
  | [] -> raise (Failure "not found")
  | (y, v)::tl -> if x = y then v else find_callee x tl

let bind_block : env -> (id * value) list -> env
= fun env vs ->
  let (xs, _) = List.split vs in
  List.fold_left (fun env x -> update_env x (VBlock (x, vs)) env) env xs

(* Argument binding *)
let rec arg_binding : env -> arg -> value -> env
= fun env arg v ->
  match (arg, v) with
  | ArgOne (x, t), _ -> update_env x v env 
  | ArgTuple xs, VTuple vs -> (try List.fold_left2 arg_binding env xs vs with _ -> raise (Failure "argument binding failure - tuples are not compatible"))
  | _ -> raise (Failure "argument binding failure")

let rec let_binding : env -> let_bind -> value -> env
= fun env x v ->
  match (x, v) with
  | BindOne x, _ -> update_env x v env
  | BindTuple xs, VTuple vs -> (try List.fold_left2 let_binding env xs vs with _ -> raise (Failure "argument binding failure - tuples are not compatible"))
  | _ -> raise (Failure "let binding failure")

(* Pattern Matching *)
let rec find_first_branch : value -> branch list -> (pat * exp)
= fun v bs -> 
  match bs with 
  | [] -> raise (Failure "Pattern matching failure")
  | (p, e)::tl -> if (pattern_match v p) then (p, e) else find_first_branch v tl

and pattern_match : value -> pat -> bool
= fun v p ->
  match (v, p) with
  | VInt n1, PInt n2 -> n1 = n2
  | VBool b1, PBool b2 -> b1 = b2
  | VList l1, PList l2 -> pattern_match_list l1 l2
  | VTuple l1, PTuple l2 -> pattern_match_list l1 l2
  | VCtor (x1, l1), PCtor (x2, l2) -> (x1 = x2) && pattern_match_list l1 l2
  | VList [], PCons (phd::ptl) -> if ptl = [] then (pattern_match v phd) else false
  | VList (vhd::vtl), PCons (phd::ptl) -> if ptl = [] then (pattern_match v phd) else (pattern_match vhd phd) && (pattern_match (VList vtl) (PCons ptl))
  (*| VList (vhd::vtl), PCons (phd, ptl) -> (pattern_match v phd) && (pattern_match (VList vtl) ptl) *)
  | _, PVar x -> true
  | _, PUnder -> true
  | _, Pats pl -> (try List.exists (pattern_match v) pl with _ -> false)
  | _ -> false

and pattern_match_list : value list -> pat list -> bool
= fun vs ps -> try List.for_all2 pattern_match vs ps with _ -> false

let rec bind_pat : env -> value -> pat -> env
= fun env v p ->
  match (v, p) with
  | _, PInt n2 -> env
  | _, PBool b2 -> env
  | _, PUnder -> env
  | _, PVar x -> update_env x v env
  | VList l1, PList l2 -> bind_pat_list env l1 l2 
  | VTuple l1, PTuple l2 -> bind_pat_list env l1 l2
  | VCtor (x1, l1), PCtor (x2, l2) -> bind_pat_list env l1 l2
  | VList [], PCons (phd::ptl) ->  if ptl = [] then (bind_pat env v phd) else raise (Failure "Pattern binding failure")
  | VList (vhd::vtl), PCons (phd::ptl) -> if ptl = [] then bind_pat env v phd else bind_pat (bind_pat env vhd phd) (VList vtl) (PCons ptl)
  (*| VList (vhd::vtl), PCons (phd::ptl) -> bind_pat (bind_pat env vhd phd) (VList vtl) ptl*)
  | _ -> raise (Failure "Pattern binding failure")

and bind_pat_list : env -> value list -> pat list -> env
= fun env vs ps -> List.fold_left2 bind_pat env vs ps

(* exp evaluation *)
let rec eval : env -> exp -> value
=fun env e ->
  if (Unix.gettimeofday() -. !start_time >0.20) then 
    let _ = (infinite_count:=!(infinite_count)+1) in
    raise TimeoutError
  else
  match e with   
  (* base *)
  | EUnit -> VUnit
  | Const n -> VInt n
  | TRUE -> VBool true
  | FALSE -> VBool false
  | String id -> VString id
  | EVar x -> lookup_env x env
  | EList es -> VList (List.map (eval env) es)
  | ETuple es -> VTuple (List.map (eval env) es)
  | ECtor (c, es) ->  VCtor (c, List.map (eval env) es)
  (* aop *)
  | ADD (e1, e2) -> (eval_abop env e1 e2 (+))
  | SUB (e1, e2) -> (eval_abop env e1 e2 (-))
  | MUL (e1, e2) -> (eval_abop env e1 e2 ( * ))
  | DIV (e1, e2) -> (eval_abop env e1 e2 (/))
  | MOD (e1, e2) -> (eval_abop env e1 e2 (mod))
  | MINUS e ->
    begin match (eval env e) with
    | VInt n -> VInt (-n)
    | _ -> raise (Failure "arithmetic_operation error")
    end
  (*bexp*)
  | NOT e -> 
    begin match (eval env e) with
    | VBool b -> VBool (not b)
    | _ -> raise (Failure "boolean_operation error")
    end
  | OR (e1, e2) -> (eval_bbop env e1 e2 (||))
  | AND (e1, e2) -> (eval_bbop env e1 e2 (&&))
  | LESS (e1, e2) -> (eval_abbop env e1 e2 (<))
  | LARGER (e1, e2) -> (eval_abbop env e1 e2 (>))
  | LESSEQ (e1, e2) -> (eval_abbop env e1 e2 (<=))
  | LARGEREQ (e1, e2) -> (eval_abbop env e1 e2 (>=))
  | EQUAL (e1, e2) -> eval_equality env e1 e2 (=)
  | NOTEQ (e1, e2) -> eval_equality env e1 e2 (!=)
  (* lop *)
  | AT (e1, e2) ->
    begin match (eval env e1, eval env e2) with
    | VList vs1, VList vs2 -> VList (vs1 @ vs2)
    | _ -> raise (Failure "list_operation error")
    end
  | DOUBLECOLON (e1, e2) ->
    let (v1,v2) = (eval env e1,eval env e2) in
    begin match v1,v2 with
    |  _,VList vs -> VList (v1::vs)
    | _ -> raise (Failure "list_operation error")
    end
  | STRCON (e1,e2) ->
    let (v1,v2) = (eval env e1,eval env e2) in
    begin match v1,v2 with
    | VString str1,VString str2 -> VString (str1^str2)
    | _ -> raise (Failure "str_equation error")
    end
  (* else *)
  | IF (e1, e2, e3) ->
    begin match (eval env e1) with
    | VBool true -> eval env e2
    | VBool false -> eval env e3
    | _ -> raise (Failure "if_type error")
    end
  | ELet (f, is_rec, args, typ, e1, e2) -> 
    begin match args with
    | [] ->
      (* Value binding *)
      if is_rec then 
        let v1 = eval env e1 in
        begin match f with
          | BindOne f -> 
            begin match v1 with
            | VFun (x, e, closure) -> eval (update_env f (VFunRec (f, x, e, closure)) env) e2
            | _ -> eval (update_env f v1 env) e2
            end
          | _ -> raise (Failure "left-hand side cannot be a tupple")
        end
      else eval (let_binding env f (eval env e1)) e2
    | _ ->
      (* Function binding *)
      let rec binding : arg list -> exp -> exp 
      = fun xs e -> 
        begin match xs with
        | [] -> e
        | hd::tl -> EFun (hd, binding tl e)
        end 
      in
      let x = List.hd args in
      let vf = 
        if is_rec then
          begin match f with
          | BindOne f -> VFunRec (f, x, (binding (List.tl args) e1), env)
          | _ -> raise (Failure "left-hand side cannot be a tupple")
          end
        else 
          VFun (x, (binding (List.tl args) e1), env)
      in
      eval (let_binding env f vf) e2
    end
  | EBlock (is_rec, bindings, e2) -> 
    let env = 
      begin match is_rec with
      | true ->
        let (func_map, const_map) = List.fold_left (
          fun (func_map, const_map) (f, is_rec, args, typ, exp) ->
          begin match f with 
          | BindOne x ->
            let v = eval env (ELet (BindOne x, is_rec, args, typ, exp, EVar x)) in
            if is_fun v then ((x, v)::func_map, const_map) else (func_map, (x, v)::const_map)
          | _ -> raise (Failure "l-value cannot be a tupple")
          end
        ) ([], []) bindings
        in
        (* constant mapping *)
        let init_env = List.fold_left (fun env (x, c) -> update_env x c env) env const_map in
        (* update each function's closure *)
        let func_map = List.map (fun (x, v) -> (x, update_closure init_env v)) func_map in
        (* block mapping *)
        List.fold_left (fun env (x, v) -> update_env x (VBlock (x, func_map)) env) init_env func_map
      | false ->
        let vs = List.map (fun (f, is_rec, args, typ, e) -> eval env (ELet (f, is_rec, args, typ, e, let_to_exp f))) bindings in
        List.fold_left2 (fun env (f, is_rec, args, typ, e) v -> let_binding env f v) env bindings vs
      end
    in eval env e2
  | EMatch (e, bs) ->
    let v = eval env e in
    let (p, ex) = find_first_branch v bs in
    eval (bind_pat env v p) ex
  | EFun (arg, e) -> VFun (arg, e, env)
  | EApp (e1, e2) ->
    let (v1, v2) = (eval env e1, eval env e2) in
    begin match v1 with
    | VFun (x, e, closure) -> eval (arg_binding closure x v2) e
    | VFunRec (f, x, e, closure) -> eval (update_env f v1 (arg_binding closure x v2)) e
    | VBlock (f, vs) ->
      let v = find_callee f vs in
      begin match v with
      | VFunRec (f, x, e, closure) -> 
        let block_env = bind_block closure vs in
        eval (arg_binding block_env x v2) e
      | _ -> raise (Failure "mutually recursive function call error")
      end
    | _ -> raise (Failure "function_call error")
    end
  | Hole n -> VHole n
  | Raise e -> 
    let e = eval env e in
    raise (EExcept e)

and eval_abop : env -> exp -> exp -> (int -> int -> int) -> value
= fun env e1 e2 op ->
  match (eval env e1, eval env e2) with
  | VInt n1, VInt n2 -> VInt (op n1 n2)
  | _ -> raise (Failure "arithmetic_operation error")

and eval_abbop : env -> exp -> exp -> (int -> int -> bool) -> value
= fun env e1 e2 op ->
  match (eval env e1, eval env e2) with
  | VInt n1, VInt n2 -> VBool (op n1 n2)
  | _ -> raise (Failure "int_relation error")

and eval_bbop : env -> exp -> exp -> (bool -> bool -> bool) -> value
= fun env e1 e2 op ->
  match (eval env e1, eval env e2) with
  | VBool b1, VBool b2 -> VBool (op b1 b2)
  | _ -> raise (Failure "boolean_operation error")

and eval_equality : env -> exp -> exp -> ('a -> 'a -> bool) -> value
= fun env e1 e2 op ->
  let (x,y) = (eval env e1, eval env e2) in
  match (x, y) with
  | VFun _, _  
  | VFunRec _, _
  | _, VFun _
  | _, VFunRec _ -> raise (Failure "Unable to check functions equality")
  | VList l1, VList l2
  | VTuple l1, VTuple l2
  | VCtor (_,l1), VCtor(_,l2) ->
    let check v = 
      match v with
      | VFun _ | VFunRec _ -> true
      | _ -> false
    in
    let b = (List.exists check l1) || (List.exists check l2) in
    if b then raise (Failure "Unable to check functions equality")
    else VBool (op x y)
  | _ -> VBool (op x y)

let rec eval_decl : env -> decl -> env
= fun env decl ->
  match decl with
  | DLet (x,is_rec,args,typ,exp) -> 
    let exp = ELet (x, is_rec, args, typ, exp, let_to_exp x) in
    let_binding env x (eval env exp)
  | DBlock (is_rec, bindings) ->
    begin match is_rec with
    | true ->
      let (func_map, const_map) = List.fold_left (
        fun (func_map, const_map) (f, is_rec, args, typ, exp) ->
        begin match f with 
        | BindOne x ->
          let v = eval env (ELet (BindOne x, is_rec, args, typ, exp, EVar x)) in
          if is_fun v then ((x, v)::func_map, const_map) else (func_map, (x, v)::const_map)
        | _ -> raise (Failure "l-value cannot be a tupple")
        end
      ) ([], []) bindings
      in
      (* constant mapping *)
      let init_env = List.fold_left (fun env (x, c) -> update_env x c env) env const_map in
      (* update each function's closure *)
      let func_map = List.map (fun (x, v) -> (x, update_closure init_env v)) func_map in
      (* block mapping *)
      List.fold_left (fun env (x, v) -> update_env x (VBlock (x, func_map)) env) init_env func_map
    | false ->
      let envs = List.map (fun binding -> eval_decl env (DLet binding)) bindings in
      List.fold_left (fun env new_env -> BatMap.union env new_env) env envs
    end
  | _ -> env

let run : prog -> env
= fun decls -> 
  count:=(!count)+1;
  start_time:=Unix.gettimeofday();
  let init_env = List.fold_left eval_decl empty_env (External.init_prog) in
  start_time:=Unix.gettimeofday();
  let env = List.fold_left eval_decl init_env decls in
  BatMap.diff env init_env


