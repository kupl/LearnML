open Lang
open Util

(* Control variables *)
let count = ref 0
let infinite_count = ref 0
let start_time = ref 0.0

(* Argument binding *)
let rec arg_binding : env -> arg -> value -> env
= fun env arg v ->
  match (arg, v) with
  | ArgOne (x, t), _ -> update_env x v env 
  | ArgTuple xs, VTuple vs -> List.fold_left2 arg_binding env xs vs
  | _ -> raise (Failure "argument binding failure")

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
  if(Unix.gettimeofday() -. !start_time >0.05) then let _ = (infinite_count:=!(infinite_count)+1) in raise (Failure "Timeout")
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
  | ADD (e1, e2) -> VInt (eval_abop env e1 e2 (+)) 
  | SUB (e1, e2) -> VInt (eval_abop env e1 e2 (-))
  | MUL (e1, e2) -> VInt (eval_abop env e1 e2 ( * ))
  | DIV (e1, e2) -> VInt (eval_abop env e1 e2 (/))
  | MOD (e1, e2) -> VInt (eval_abop env e1 e2 (mod))
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
  | OR (e1, e2) -> VBool (eval_bbop env e1 e2 (||))
  | AND (e1, e2) -> VBool (eval_bbop env e1 e2 (&&))
  | LESS (e1, e2) -> VBool (eval_abbop env e1 e2 (<))
  | LARGER (e1, e2) -> VBool (eval_abbop env e1 e2 (>))
  | LESSEQ (e1, e2) -> VBool (eval_abbop env e1 e2 (<=))
  | LARGEREQ (e1, e2) -> VBool (eval_abbop env e1 e2 (>=))
  | EQUAL (e1, e2) -> VBool ((eval env e1) = (eval env e2))
  | NOTEQ (e1, e2) -> VBool ((eval env e1) = (eval env e2))
  (* lop *)
  | AT (e1, e2) ->
    begin match (eval env e1, eval env e2) with
    | VList vs1, VList vs2 -> VList (vs1 @ vs2)
    | _ -> raise (Failure "list_operation error")
    end
  | DOUBLECOLON (e1, e2) ->
    begin match (eval env e2) with
    | VList vs -> VList ((eval env e1)::vs) 
    | _ -> raise (Failure "list_operation error")
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
        begin match v1 with
        | VFun (x, e, closure) -> eval (update_env f (VFunRec (f, x, e, closure)) env) e2
        | _ -> eval (update_env f v1 env) e2
        end
      else eval (update_env f (eval env e1) env) e2
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
          VFunRec (f, x, (binding (List.tl args) e1), env)
        else 
          VFun (x, (binding (List.tl args) e1), env)
      in
      eval (update_env f vf env) e2
    end
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
    | _ -> raise (Failure "function_call error")
    end
  | Hole n -> VHole n

and eval_abop : env -> exp -> exp -> (int -> int -> int) -> int
= fun env e1 e2 op ->
  match (eval env e1, eval env e2) with
  | VInt n1, VInt n2 -> op n1 n2
  | _ -> raise (Failure "arithmetic_operation error")

and eval_abbop : env -> exp -> exp -> (int -> int -> bool) -> bool
= fun env e1 e2 op ->
  match (eval env e1, eval env e2) with
  | VInt n1, VInt n2 -> op n1 n2
  | _ -> raise (Failure "int_relation error")

and eval_bbop : env -> exp -> exp -> (bool -> bool -> bool) -> bool
= fun env e1 e2 op ->
  match (eval env e1, eval env e2) with
  | VBool b1, VBool b2 -> op b1 b2
  | _ -> raise (Failure "boolean_operation error")
    
let eval_decl : decl -> env -> env
=fun decl env -> 
  match decl with 
  | DData _ -> env
  | DLet (x,is_rec,args,typ,exp) -> 
    let exp = ELet (x, is_rec, args, typ, exp, EVar x) in
    update_env x (eval env exp) env

let run : prog -> env
= fun decls -> 
  start_time:=Unix.gettimeofday(); 
  count:=(!count)+1;
  (list_fold eval_decl decls empty_env)

