open Lang
open Util

(* Control variables *)
let count = ref 0
let infinite_count = ref 0
let start_time = ref 0.0
let timeout = ref 0.2

let trace_set = ref []
let trace_option = ref false

let vhole_num = ref 0
let fresh_vhole () = (vhole_num := !vhole_num - 1; VHole (!vhole_num))

let rec is_hole : value -> bool
= fun v ->
  match v with
  | VList vs | VTuple vs | VCtor (_, vs) -> List.exists is_hole vs
  | VHole _ -> true
  | _ -> false

let rec gen_hole_list : int -> value list
= fun n ->
  if n = 0 then []
  else
    (fresh_vhole ())::(gen_hole_list (n-1)) 
    
(* Block task *)
let rec is_fun : value -> bool
= fun v ->
  match v with
  | VList vs | VTuple vs | VCtor (_, vs) -> List.exists is_fun vs
  | VFun _ | VFunRec _ | VBlock _ -> true
  | _ -> false

let rec update_closure : env -> value -> value
= fun env v ->
  match v with
  | VList vs -> VList (List.map (update_closure env) vs)
  | VTuple vs -> VTuple (List.map (update_closure env) vs)
  | VCtor (x, vs) -> VCtor (x, List.map (update_closure env) vs)
  | VFun (x, e, closure) -> VFun (x, e, env)
  | VFunRec (f, x, e, closure) -> VFunRec (f, x, e, env)
  | VBlock (f, vs) -> 
    let (xs, vs) = List.split vs in
    let vs = List.map (update_closure env) vs in
    VBlock (f, List.combine xs vs)
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
  | ArgUnder t, _ -> env
  | ArgOne (x, t), _ -> update_env x v env 
  | ArgTuple xs, VTuple vs -> 
    (
      try List.fold_left2 arg_binding env xs vs 
      with 
      | Invalid_argument _ -> raise (Failure "argument binding failure - tuples are not compatible")
      | _ -> raise (StackOverflow "Stack overflow during evaluation (looping recursion?)")
    )
  | ArgTuple xs, VHole _ -> arg_binding env arg (VTuple (gen_hole_list (List.length xs)))
  | _ -> raise (Failure "argument binding failure")

let rec let_binding : env -> let_bind -> value -> env
= fun env x v ->
  match (x, v) with
  | BindUnder, _ -> env
  | BindOne x, _ -> update_env x v env
  | BindTuple xs, VTuple vs -> 
    (
      try List.fold_left2 let_binding env xs vs 
      with 
      | Invalid_argument _ -> raise (Failure "argument binding failure - tuples are not compatible")
      | _ -> raise (StackOverflow "Stack overflow during evaluation (looping recursion?)")
    )
  | BindTuple xs, VHole _ -> let_binding env x (VTuple (gen_hole_list (List.length xs)))
	| _ -> raise (Failure "let binding failure")

(* Pattern Matching *)
let rec find_first_branch : value -> branch list -> (pat * lexp)
= fun v bs -> 
  match bs with 
  | [] -> raise (Failure "Pattern matching failure")
  | (p, e)::tl -> if (pattern_match v p) then (p, e) else find_first_branch v tl

and pattern_match : value -> pat -> bool
= fun v p ->
  match (v, p) with
  | VInt n1, PInt n2 -> n1 = n2
  | VBool b1, PBool b2 -> b1 = b2
  | VList l1, PList l2 | VTuple l1, PTuple l2 -> pattern_match_list l1 l2
  | VCtor (x1, l1), PCtor (x2, l2) -> (x1 = x2) && pattern_match_list l1 l2
  | VList [], PCons (phd::ptl) -> if ptl = [] then (pattern_match v phd) else false
  | VList (vhd::vtl), PCons (phd::ptl) -> if ptl = [] then (pattern_match v phd) else (pattern_match vhd phd) && (pattern_match (VList vtl) (PCons ptl))
  (*| VList (vhd::vtl), PCons (phd, ptl) -> (pattern_match v phd) && (pattern_match (VList vtl) ptl) *)
  | _, Pats pl -> (try List.exists (pattern_match v) pl with _ -> false)
  | _, PVar _ | _, PUnder | VHole _, _ -> true
  | _ -> false

and pattern_match_list : value list -> pat list -> bool
= fun vs ps -> try List.for_all2 pattern_match vs ps with _ -> false

let rec check_patterns : pat list -> bool
= fun ps ->
  let var_set_list = List.map (gather_vars BatSet.empty) ps in
  let base = List.hd var_set_list in
  List.for_all (fun set -> BatSet.equal set base) var_set_list

and gather_vars : id BatSet.t -> pat -> id BatSet.t
= fun set p ->
  match p with
  | PVar x -> BatSet.add x set
  | PList ps | PTuple ps | PCtor (_, ps) | PCons ps | Pats ps -> List.fold_left gather_vars set ps
  | _ -> set

let rec bind_pat : env -> value -> pat -> env
= fun env v p ->
  match (v, p) with
  | _, PInt _ | _, PBool _ | _, PUnder -> env
  | VList l1, PList l2 | VTuple l1, PTuple l2 | VCtor (_, l1), PCtor (_, l2) -> bind_pat_list env l1 l2
  | VHole _, PList l2 | VHole _, PTuple l2 | VHole _, PCtor (_, l2) -> bind_pat_list env (gen_hole_list (List.length l2)) l2
  | VList [], PCons (phd::ptl) ->  if ptl = [] then (bind_pat env v phd) else raise (Failure "Pattern binding failure")
  | VList (vhd::vtl), PCons (phd::ptl) -> if ptl = [] then bind_pat env v phd else bind_pat (bind_pat env vhd phd) (VList vtl) (PCons ptl)
  | _, PVar x -> update_env x v env
  | _, Pats ps -> if check_patterns ps then bind_pat env v (List.find (pattern_match v) ps) else raise (Failure "Invalid pattern list")
  (*| VList (vhd::vtl), PCons (phd::ptl) -> bind_pat (bind_pat env vhd phd) (VList vtl) ptl*)
  | _ -> raise (Failure ("Pattern binding failure : " ^ Print.pat_to_string p ^ ", " ^ Print.value_to_string v))

and bind_pat_list : env -> value list -> pat list -> env
= fun env vs ps -> List.fold_left2 bind_pat env vs ps

let rec value_equality : value -> value -> bool
= fun v1 v2 ->
  match v1,v2 with
  | VBool b1,VBool b2 -> b1=b2
  | VInt n1,VInt n2 -> n1=n2
  | VString id1,VString id2 -> id1=id2
  | VList l1, VList l2
  | VTuple l1, VTuple l2 -> (try List.for_all2 value_equality l1 l2 with _ -> raise EqualError)
  | VCtor (x1,l1), VCtor(x2,l2) -> (try ((x1=x2) && List.for_all2 value_equality l1 l2) with _ -> raise EqualError)
  | _ -> raise EqualError

(* exp evaluation *)
let rec eval : env -> lexp -> value
= fun env (l,e) ->
  let _ = if !trace_option then trace_set := l::(!trace_set) in
  if (Unix.gettimeofday() -. !start_time > !timeout) then 
    let _ = (infinite_count:=!(infinite_count)+1) in
    raise TimeoutError
  else
  match e with   
  (* base *)
  | SInt n -> VHole n
  | SStr n -> VHole n
  | EUnit -> VUnit
  | Const n -> VInt n
  | TRUE -> VBool true
  | FALSE -> VBool false
  | String id -> VString id
  | EVar x -> (try lookup_env x env with _ -> raise (Failure (x ^ " Not found")))
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
    | VHole _ -> fresh_vhole ()
    | _ -> raise (Failure "arithmetic_operation error")
    end
  (*bexp*)
  | NOT e -> 
    begin match (eval env e) with
    | VBool b -> VBool (not b)
    | VHole _ -> fresh_vhole ()
    | _ -> raise (Failure "boolean_operation error")
    end
  | OR (e1, e2) -> (eval_bbop env e1 e2 (||))
  | AND (e1, e2) -> (eval_bbop env e1 e2 (&&))
  | LESS (e1, e2) -> (eval_abbop env e1 e2 (<))
  | LARGER (e1, e2) -> (eval_abbop env e1 e2 (>))
  | LESSEQ (e1, e2) -> (eval_abbop env e1 e2 (<=))
  | LARGEREQ (e1, e2) -> (eval_abbop env e1 e2 (>=))
  | EQUAL (e1, e2) -> 
    begin match (eval env e1, eval env e2) with
    | VHole n, _ | _, VHole n -> fresh_vhole ()
    | v1, v2 -> VBool (try value_equality v1 v2 with _ -> false)
    end
  | NOTEQ (e1, e2) -> 
    begin match (eval env e1, eval env e2) with
    | VHole n, _ | _, VHole n -> fresh_vhole ()
    | v1, v2 -> VBool (not (try value_equality v1 v2 with _ -> false))
    end
  (* lop *)
  | AT (e1, e2) ->
    begin match (eval env e1, eval env e2) with
    | VList vs1, VList vs2 -> VList (vs1 @ vs2)
    | VHole n, _ | _, VHole n -> fresh_vhole ()
    | _ -> raise (Failure "list_operation error")
    end
  | DOUBLECOLON (e1, e2) ->
    begin match (eval env e1, eval env e2) with
    | v1, VList vs -> VList (v1::vs)
    | VHole n, _ | _, VHole n -> fresh_vhole ()
    | _ -> raise (Failure "list_operation error")
    end
  | STRCON (e1,e2) ->
    begin match (eval env e1, eval env e2) with
    | VString str1, VString str2 -> VString (str1^str2)
    | VHole n, _ | _, VHole n -> fresh_vhole ()
    | _ -> raise (Failure "str_equation error")
    end
  (* else *)
  | IF (e1, e2, e3) ->
    begin match (eval env e1) with
    | VBool true -> eval env e2
    | VBool false -> eval env e3
    | VHole _ -> fresh_vhole ()
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
          | _ -> raise (Failure "Only variables are allowed as left-hand side of `let rec'")
        end
      else eval (let_binding env f (eval env e1)) e2
    | _ ->
      (* Function binding *)
      let rec binding : arg list -> lexp -> lexp 
      = fun xs e -> 
        begin match xs with
        | [] -> e
        | hd::tl -> (l, EFun (hd, binding tl e))
        end 
      in
      let x = List.hd args in
      let vf = 
        if is_rec then
          begin match f with
          | BindOne f -> VFunRec (f, x, (binding (List.tl args) e1), env)
          | _ -> raise (Failure "Only variables are allowed as left-hand side of `let rec'")
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
            let v = eval env (gen_label(), ELet (BindOne x, is_rec, args, typ, exp, (gen_label(),EVar x))) in
            if is_fun v then ((x, v)::func_map, const_map) else (func_map, (x, v)::const_map)
          | _ -> raise (Failure "Only variables are allowed as left-hand side of `let rec'")
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
        let vs = List.map (fun (f, is_rec, args, typ, e) -> eval env (gen_label(),ELet (f, is_rec, args, typ, e, let_to_exp f))) bindings in
        List.fold_left2 (fun env (f, is_rec, args, typ, e) v -> let_binding env f v) env bindings vs
      end
    in eval env e2
  | EMatch (e, bs) ->
    let v = eval env e in
    if is_hole v then fresh_vhole ()
    else 
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
    | VHole _ -> fresh_vhole ()
    | _ -> raise (Failure "function_call error")
    end
  | Hole n -> VHole n
  | Raise e -> 
    let e = eval env e in
    raise (EExcept e)

and eval_abop : env -> lexp -> lexp -> (int -> int -> int) -> value
= fun env e1 e2 op ->
  match (eval env e1, eval env e2) with
  | VInt n1, VInt n2 -> VInt (op n1 n2)
  | VHole n, _ | _, VHole n -> fresh_vhole ()
  | _ -> raise (Failure "arithmetic_operation error")

and eval_abbop : env -> lexp -> lexp -> (int -> int -> bool) -> value
= fun env e1 e2 op ->
  match (eval env e1, eval env e2) with
  | VInt n1, VInt n2 -> VBool (op n1 n2)
  | VHole n, _ | _, VHole n -> fresh_vhole ()
  | _ -> raise (Failure "int_relation error")

and eval_bbop : env -> lexp -> lexp -> (bool -> bool -> bool) -> value
= fun env e1 e2 op ->
  match (eval env e1, eval env e2) with
  | VBool b1, VBool b2 -> VBool (op b1 b2)
  | VHole n, _ | _, VHole n -> fresh_vhole ()
  | _ -> raise (Failure "boolean_operation error")
  
let rec eval_decl : env -> decl -> env
= fun env decl ->
  match decl with
  | DLet (x,is_rec,args,typ,exp) -> 
    let exp = 
      begin match x with 
      | BindUnder -> exp
      | _ -> (gen_label(),ELet (x, is_rec, args, typ, exp, let_to_exp x))
      end
    in
    let_binding env x (eval env exp)
  | DBlock (is_rec, bindings) ->
    begin match is_rec with
    | true ->
      let (func_map, const_map) = List.fold_left (
        fun (func_map, const_map) (f, is_rec, args, typ, exp) ->
        begin match f with 
        | BindOne x ->
          let v = eval env (gen_label(),(ELet (BindOne x, is_rec, args, typ, exp, (gen_label(),EVar x)))) in
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
  let init_env = List.fold_left eval_decl empty_env (!init_pgm) in
  start_time:=Unix.gettimeofday();
  (if !trace_option then trace_set:=[]);
  let env = List.fold_left eval_decl init_env decls in
  BatMap.diff env init_env

(* Utility functions *)
let rec get_output : prog -> input -> value
= fun pgm input ->
  try
    let res_var = "__res__" in
    let pgm = pgm@(!grading_pgm) in
    let pgm' = pgm @ [(DLet (BindOne res_var,false,[],fresh_tvar(),(appify (gen_label(), EVar !Options.opt_entry_func) input)))] in
    let env = run pgm' in
    lookup_env res_var env
  with e -> raise e

let rec is_solution : prog -> examples -> bool
= fun pgm examples ->
  List.for_all (fun (input, output) ->
    try
      value_equality (get_output pgm input) output
    with _ -> false
  ) examples