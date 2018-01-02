open Lang
open Util

(*
let rec find_first_branch : value -> branch list -> branch 
= fun v bs ->
  match bs with  
  | [] -> raise (Failure "Pattern matching failure")
  | (p, ex)::tl ->
    begin match (v, p) with
    | (VInt n1, PInt n2) -> if (n1 = n2) then (p, ex) else find_first_branch v tl
    | (VBool b1, PBool b2) -> if (b1 = b2) then (p, ex) else find_first_branch v tl
    | (VList l1, PList l2) -> if (list_equal l1 l2) then (p, ex) else find_first_branch v tl
    | (VTuple l1, PTuple l2) -> if (list_equal l1 l2) then (p, ex) else find_first_branch v tl
    | (VCtor (x1, l1), PCtor(x2, l2)) -> if ((x1 = x2) && (list_equal l1 l2)) then (p, ex) else find_first_branch v tl
    | (VList l1, PCons l2) -> if (cons_equal l1 l2) then (p, ex) else find_first_branch v tl
    | (_, PVar x) -> (p, ex)
    | (_, PUnder) -> (p, ex)
    | (_, Pats pl) -> find_first_branch v ((pats_to_branch pl ex)@tl)
    | _ -> find_first_branch v tl
    end

and list_equal : value list -> pat list -> bool
= fun vs ps ->
  match (vs, ps) with
  | ([], []) -> true
  | ([], _) -> false
  | (_, []) -> false
  | (vhd::vtl, phd::ptl) ->
    begin match (vhd, phd) with 
    | (VInt n1, PInt n2) -> if (n1 = n2) then list_equal vtl ptl else false
    | (VBool b1, PBool b2) -> if (b1 = b2) then list_equal vtl ptl else false
    | (VList l1, PList l2) -> if (list_equal l1 l2) then list_equal vtl ptl else false
    | (VTuple l1, PTuple l2) -> if (list_equal l1 l2) then list_equal vtl ptl else false
    | (VCtor (x1, l1), PCtor(x2, l2)) -> if ((x1 = x2) && (list_equal l1 l2)) then list_equal vtl ptl else false
    | (VList l1, PCons l2) -> if (cons_equal l1 l2) then list_equal vtl ptl else false
    | (_, PVar x) -> list_equal vtl ptl
    | (_, PUnder) -> list_equal vtl ptl
    | _ -> false
    end

and cons_equal : value list -> pat list -> bool 
= fun vs ps ->
  match (vs, ps) with
  | ([], []) -> false
  | ([], [p]) ->
    begin match p with
    | PList [] -> true
    | PVar x -> true
    | PUnder -> true
    | _ -> false
    end
  | ([v], [p]) -> 
    begin match p with
    | PList ps -> list_equal [v] ps
    | PVar x -> true
    | PUnder -> true
    | _ -> false
    end
  | (vhd::vtl, []) -> false
  | (vhd::vtl, phd::ptl) ->
    begin match (vhd, phd) with
    | (VInt n1, PInt n2) -> if (n1 = n2) then cons_equal vtl ptl else false
    | (VBool b1, PBool b2) -> if (b1 = b2) then cons_equal vtl ptl else false
    | (VList l1, PList l2) -> if (list_equal l1 l2) then cons_equal vtl ptl else false
    | (VTuple l1, PTuple l2) -> if (list_equal l1 l2) then cons_equal vtl ptl else false
    | (VCtor (x1, l1), PCtor(x2, l2)) -> if ((x1 = x2) && (list_equal l1 l2)) then cons_equal vtl ptl else false
    | (VList l1, PCons l2) -> if (cons_equal l1 l2) then cons_equal vtl ptl else false
    | (_, PVar x) -> if (ptl = []) then true else cons_equal vtl ptl
    | (_, PUnder) -> cons_equal vtl ptl
    | _ -> false
    end
  | _ -> false

and pats_to_branch : pat list -> exp -> branch list
= fun ps ex ->
  match ps with
  | [] -> []
  | hd::tl -> (hd, ex)::(pats_to_branch tl ex)

let rec env_of_pat : value -> pat -> env -> env
= fun v p env ->
  match (v, p) with
  | (VInt n1, PInt n2) -> env
  | (VBool b1, PBool b2) -> env
  | (_, PVar x) -> update_env x v env 
  | (VList vs, PList ps) -> env_of_pat_list vs ps env
  | (VTuple vs, PTuple ps) -> env_of_pat_list vs ps env
  | (VCtor (x1, vs), PCtor (x2, ps))-> env_of_pat_list vs ps env
  | (VList vs, PCons ps) -> env_of_cons vs ps env
  | (_, PUnder) -> env
  | _ -> raise (Failure "Pattern matching failure")

and env_of_pat_list : value list -> pat list -> env -> env
= fun vs ps env ->
  match (vs, ps) with
  | ([], []) -> env
  | (vhd::vtl, phd::ptl) -> 
    let new_env = env_of_pat vhd phd env in
    env_of_pat_list vtl ptl new_env
  | _ -> raise (Failure "Pattern matching failure")

and env_of_cons : value list -> pat list -> env -> env
= fun vs ps env ->
  match (vs, ps) with
  | ([], [p]) ->
    begin match p with
    | PList [] -> env
    | PVar x -> update_env x (VList []) env
    | PUnder -> env
    | _ -> raise (Failure "Pattern matching failure")
    end
  | ([v], [p]) -> 
    begin match p with
    | PList ps -> env_of_pat_list [v] ps env
    | PVar x -> update_env x (VList [v]) env
    | PUnder -> env
    | _ -> raise (Failure "Pattern matching failure")
    end
  | (vhd::vtl, phd::ptl) -> 
    begin match phd with
    | PVar x ->
      if ptl = [] then 
        update_env x (VList vs) env
      else 
        let new_env = env_of_pat vhd phd env in
        env_of_cons vtl ptl new_env
    | _ ->
      let new_env = env_of_pat vhd phd env in
      env_of_cons vtl ptl new_env
    end
  | _ -> raise (Failure "Pattern matching failure")
*)

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
  | _ -> raise (Failure "Pattern binding failure")

and bind_pat_list : env -> value list -> pat list -> env
= fun env vs ps -> List.fold_left2 bind_pat env vs ps

let count = ref 0
let infinite_count = ref 0
let start_time = ref 0.0

(* exp evaluation *)
let rec eval : env -> exp -> value
=fun env e -> 
  if(Unix.gettimeofday() -. !start_time >0.05) then let _ = (infinite_count:=!(infinite_count)+1) in raise (Failure "Timeout")
  else
  match e with   
  (* base *)
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
      let (x1, t1) = List.hd args in
      let vf = 
        if is_rec then
          VFunRec (f, x1, (binding (List.tl args) e1), env)
        else 
          VFun (x1, (binding (List.tl args) e1), env)
      in
      eval (update_env f vf env) e2
    end
  | EMatch (e, bs) ->
    let v = eval env e in
    let (p, ex) = find_first_branch v bs in
    eval (bind_pat env v p) ex
  | EFun ((x, _), e) -> VFun (x, e, env)  
  | EApp (e1, e2) ->
    begin match (eval env e1) with
    | VFun (x, e, closure) -> eval (update_env x (eval env e2) closure) e
    | VFunRec (f, x, e, closure) -> eval (update_env f (VFunRec (f,x,e,closure)) (update_env x (eval env e2) closure)) e
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

let run : prog -> bool -> env
=fun decls env_trace_option-> 
  start_time:=Unix.gettimeofday(); 
  count:=(!count)+1;
  (list_fold eval_decl decls empty_env)

