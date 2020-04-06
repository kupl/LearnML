open Lang
open Util
open Print 

module T = Type_annotate

(***************************)
(* Call_graph construction *)
(***************************)

(* Assume that all identifiers are uniquely defined => kill is needless *)
type func_id = int
type node = (id, (func_id * arg BatSet.t * typ * lexp)) BatMap.t (* function name -> (parameter info, output type) *)
type edge = (func_id * func_id, (label * ctx) BatSet.t) BatMap.t (* (caller, callee) -> (label, ctx) *)
and ctx = lexp list (* path condition *)

(* G = (V, E, entry) *)
type graph = node * edge * func_id

type env = (id, func_id) BatMap.t (* Var -> func_id *)

(* Preprocessing : convert all function definitions "f = fun x -> e" into "f (x) = e" form *)
let rec get_output_typ : typ -> typ
= fun typ ->
  match typ with
  | TArr (t1, t2) -> get_output_typ t2
  | _ -> typ

let rec preprocess_exp : lexp -> lexp 
= fun (l, exp) ->
  let exp = 
    match exp with
    | EList es -> EList (List.map (fun e -> preprocess_exp e) es)
    | ECtor (x, es) -> ECtor (x, (List.map (fun e -> preprocess_exp e) es))
    | ETuple es -> ETuple (List.map (fun e -> preprocess_exp e) es)
    | EFun (arg, e) -> EFun (arg, preprocess_exp e)
    | MINUS e -> MINUS (preprocess_exp e)
    | NOT e -> NOT (preprocess_exp e)
    | ADD (e1, e2) -> ADD (preprocess_exp e1, preprocess_exp e2)
    | SUB (e1, e2) -> SUB (preprocess_exp e1, preprocess_exp e2)
    | MUL (e1, e2) -> MUL (preprocess_exp e1, preprocess_exp e2)
    | DIV (e1, e2) -> DIV (preprocess_exp e1, preprocess_exp e2)
    | MOD (e1, e2) -> MOD (preprocess_exp e1, preprocess_exp e2)
    | OR (e1, e2) -> OR (preprocess_exp e1, preprocess_exp e2)
    | AND (e1, e2) -> AND (preprocess_exp e1, preprocess_exp e2) 
    | LESS (e1, e2) -> LESS (preprocess_exp e1, preprocess_exp e2) 
    | LARGER (e1, e2) -> LARGER (preprocess_exp e1, preprocess_exp e2) 
    | LESSEQ (e1, e2) -> LESSEQ (preprocess_exp e1, preprocess_exp e2) 
    | LARGEREQ (e1, e2) -> LARGEREQ (preprocess_exp e1, preprocess_exp e2)
    | EQUAL (e1, e2) -> EQUAL (preprocess_exp e1, preprocess_exp e2)
    | NOTEQ (e1, e2) -> NOTEQ (preprocess_exp e1, preprocess_exp e2)
    | AT (e1, e2) -> AT (preprocess_exp e1, preprocess_exp e2) 
    | DOUBLECOLON (e1, e2) -> DOUBLECOLON (preprocess_exp e1, preprocess_exp e2) 
    | STRCON (e1, e2) -> STRCON (preprocess_exp e1, preprocess_exp e2) 
    | EApp (e1, e2) -> EApp (preprocess_exp e1, preprocess_exp e2)
    | ELet (f, is_rec, args, typ, e1, e2) ->
      let (f, is_rec, args, typ, e1) = preprocess_binding (f, is_rec, args, typ, e1) in
      ELet (f, is_rec, args, typ, e1, preprocess_exp e2)
    | EBlock (is_rec, ds, e) -> EBlock (is_rec, List.map (fun binding -> preprocess_binding binding) ds, preprocess_exp e)
    | EMatch (e, bs) -> EMatch (preprocess_exp e, List.map (fun (p, e) -> (p, preprocess_exp e)) bs)
    | IF (e1, e2, e3) -> IF (preprocess_exp e1, preprocess_exp e2, preprocess_exp e3)
    | _ -> exp 
  in
  (l, exp)

and preprocess_binding : binding -> binding
= fun (f, is_rec, args, typ, e) ->
  let (args', e') = extract_func (preprocess_exp e) in
  (f, is_rec, args@args', get_output_typ typ, e')

and extract_func : lexp -> arg list * lexp
= fun (l, exp) ->
  match exp with
  | EFun (arg, e) -> 
    let (args, e) = extract_func e in
    (arg::args, e)
  | ELet (f, is_rec, args, typ, e1, e2) ->
    let (f, is_rec, args, typ, e1) = preprocess_binding (f, is_rec, args, typ, e1) in
    let (args', e2) = extract_func e2 in
    (args', (l, ELet (f, is_rec, args, typ, e1, e2)))
  | EBlock (is_rec, ds, e) -> 
    let ds = List.map (fun binding -> preprocess_binding binding) ds in
    let (args, e) = extract_func e in
    (args, (l, EBlock (is_rec, ds, e)))
  | _ -> ([], (l, exp))

let rec preprocess_decl : decl -> decl 
= fun decl ->
  match decl with
  | DLet binding -> DLet (preprocess_binding binding)
  | DBlock (is_rec, bindings) -> DBlock (is_rec, List.map (fun binding -> preprocess_binding binding) bindings)
  | _ -> decl

let preprocess : prog -> prog
= fun pgm -> List.map (fun decl -> preprocess_decl decl) pgm

(* Utility functions *)
let id_num = ref 0
let fresh_id () = id_num := !id_num + 1; !id_num

let get_node : graph -> node
= fun (node, edge, entry) -> node 

let get_edge : graph -> edge
= fun (node, edge, entry) -> edge 

let get_entry : graph -> func_id
= fun (node, edge, entry) -> entry 

let rec get_reachable : func_id -> graph -> func_id BatSet.t
= fun id (node, edge, entry) ->
  let rec iter reachable = 
    let reachable' = BatSet.fold (fun id acc -> 
      BatMap.foldi (fun (s, t) _ acc -> if s = id then BatSet.add t acc else acc) edge acc
    ) reachable BatSet.empty in
    if BatSet.equal reachable reachable' then reachable' else iter reachable'
  in
  iter (BatSet.singleton id)

let rec get_caller : func_id -> graph -> func_id BatSet.t
= fun id (node, edge, entry) -> BatMap.foldi (fun (s, t) _ acc -> if t = id then BatSet.add s acc else acc) edge BatSet.empty

let rec get_function_name : func_id -> graph -> id
= fun id (node, edge, entry) -> let (name, _) = List.find (fun (name, (id', _, _, _)) -> id = id') (BatMap.bindings node) in name

let rec get_entry_name : graph -> id
= fun graph -> get_function_name (get_entry graph) graph

let rec get_caller_by_name : id -> graph -> id BatSet.t
= fun id graph ->
  let (func_id, _, _, _) = BatMap.find id (get_node graph) in 
  BatSet.map (fun func_id -> get_function_name func_id graph) (get_caller func_id graph)

(* To string *)
let string_of_node : node -> string
= fun node -> string_of_map ~sep:",\n\n" (fun x -> x) 
                            (fun (id, args, typ, body) -> "(" ^ string_of_int id ^ " : \n" ^ 
                                                    "Input : " ^ string_of_set arg_to_string args ^ "\n" ^
                                                    "Output : " ^ type_to_string typ ^ "\n" ^
                                                    "Body : " ^ Print.exp_to_string body
                            ) node

let string_of_ctx : label * ctx -> string
= fun (l, ctx) -> (string_of_int l) ^ " : " ^ (pp_list exp_to_string ctx)

let string_of_edge : edge -> string
= fun edge -> 
  string_of_map (fun (s, t) -> "(" ^ string_of_int s ^ ", " ^ string_of_int t ^ ")") 
                (fun ctxs -> "" (*string_of_set string_of_ctx ctxs*)) edge

let print_graph : graph -> unit
= fun (node, edge, func_id) ->
  print_endline ("Node : "); print_endline (string_of_node node);
  print_endline ("Edge : "); print_endline (string_of_edge edge);
  print_endline ("Starting : " ^ string_of_int func_id)

(* Preprocessing *)
let rec flatten_branch : branch list -> branch list
= fun bs ->
  match bs with
  | [] -> []
  | (p, e)::tl ->
    begin match p with
    | Pats ps -> 
      let flat_bs = List.map (fun p -> (p, e)) ps in
      (flatten_branch flat_bs)@(flatten_branch tl)
    | _ -> (p, e)::(flatten_branch tl) 
    end
    
let rec is_fun : typ -> bool
= fun typ ->
  match typ with
  | TArr _ -> true
  | TList typ -> is_fun typ
  | TTuple ts -> List.exists is_fun ts
  | TCtor (tname, ts) -> (is_fun tname) || (List.exists is_fun ts)
  | _ -> false

(* Input, output type *)
let rec get_intput_args : arg list -> arg BatSet.t
= fun args -> List.fold_left (fun args arg -> BatSet.add arg args) BatSet.empty args

let rec extract_input_args : arg BatSet.t -> lexp -> arg BatSet.t
= fun args (l, exp) ->
  match exp with
  | EFun (arg, e) -> extract_input_args (BatSet.add arg args) e
  | ELet (_, _, _, _, _, e) | EBlock (_, _, e) -> extract_input_args args e
  | _ -> args

let rec get_output_typ : typ -> typ
= fun typ ->
  match typ with
  | TArr (t1, t2) -> get_output_typ t2
  | _ -> typ

(* Kill Environment *)
let kill_env x env = BatMap.remove x env

let rec kill_env_by_arg : arg -> env -> env
= fun arg env ->
  match arg with
  | ArgOne (x, typ) -> kill_env x env
  | ArgTuple xs -> List.fold_left (fun env arg -> kill_env_by_arg arg env) env xs
  | _ -> env

let rec kill_env_by_binding : let_bind -> env -> env
= fun binding env ->
  match binding with
  | BindOne x -> kill_env x env
  | BindTuple bs -> List.fold_left (fun env binding -> kill_env_by_binding binding env) env bs
  | _ -> env

let rec kill_env_by_pat : pat -> env -> env
= fun p env ->
  match p with
  | PUnit | PInt _ | PBool _ | PUnder -> env
  | PVar x -> kill_env x env
  | PList ps | PCons ps | PTuple ps | PCtor (_, ps) -> List.fold_left (fun env p -> kill_env_by_pat p env) env ps
  | _ -> raise (Failure ("Call graph : invalid patten (" ^ pat_to_string p))

(* Bindings *)
let rec bindings_to_exp : binding list -> lexp -> lexp
= fun bindings e2 ->
  match bindings with
  | [] -> e2
  | (f, is_rec, args, typ, e)::bs -> (gen_label (), ELet (f, is_rec, args, typ, e, bindings_to_exp bs e2))

(* Pattern match *)
let rec pat_to_exp : pat -> lexp
= fun p ->
  match p with
  | PUnit -> (gen_label (), EUnit)
  | PInt n -> (gen_label (), Const n)
  | PBool b -> (gen_label (), if b = true then TRUE else FALSE)
  | PVar x -> (gen_label (), EVar x)
  | PList ps -> (gen_label (), EList (List.map pat_to_exp ps))
  | PCons [p] -> pat_to_exp p
  | PCons (phd::ptl) -> (gen_label (), DOUBLECOLON (pat_to_exp phd, pat_to_exp (PCons ptl)))
  | PTuple ps -> (gen_label (), ETuple (List.map pat_to_exp ps))
  | PCtor (x, ps) -> (gen_label (), ECtor (x, List.map pat_to_exp ps))
  | PUnder -> gen_labeled_hole () (* Encoding needed *)
  | _ -> raise (Failure ("Call graph : invalid patten (" ^ pat_to_string p ^ ") to exp"))

(* 
  TODO : 
  1. update context when binding a varaible?? 
  2. generalize function definition with tuple?? 
*)

(* Extract functions body by unrolling the nested function definition *)
let rec extract_body : lexp -> lexp
= fun (l, exp) ->
  match exp with 
  | EUnit | Const _ | TRUE | FALSE | String _ | Raise _ | EVar _ -> (l, exp)
  | EList es -> (l, EList (List.map (fun e -> extract_body e) es))
  | ECtor (x, es) -> (l, ECtor (x, List.map (fun e -> extract_body e) es))
  | ETuple es -> (l, ETuple (List.map (fun e -> extract_body e) es))
  | EFun (arg, e) -> (l, EFun (arg, extract_body e))
  | MINUS e -> (l, MINUS (extract_body e))
  | NOT e -> (l, NOT (extract_body e))
  | ADD (e1, e2) -> (l, ADD (extract_body e1, extract_body e2))
  | SUB (e1, e2) -> (l, SUB (extract_body e1, extract_body e2))
  | MUL (e1, e2) -> (l, MUL (extract_body e1, extract_body e2) )
  | DIV (e1, e2) -> (l, DIV (extract_body e1, extract_body e2))
  | MOD (e1, e2) -> (l, MOD (extract_body e1, extract_body e2))
  | OR (e1, e2) -> (l, OR (extract_body e1, extract_body e2))
  | AND (e1, e2) -> (l, AND (extract_body e1, extract_body e2))
  | LESS (e1, e2) -> (l, LESS (extract_body e1, extract_body e2))
  | LARGER (e1, e2) -> (l, LARGER (extract_body e1, extract_body e2))
  | LESSEQ (e1, e2) -> (l, LESSEQ (extract_body e1, extract_body e2) )
  | LARGEREQ (e1, e2) -> (l, LARGEREQ (extract_body e1, extract_body e2))
  | EQUAL (e1, e2) -> (l, EQUAL (extract_body e1, extract_body e2))
  | NOTEQ (e1, e2) -> (l, NOTEQ (extract_body e1, extract_body e2))
  | AT (e1, e2) -> (l, AT (extract_body e1, extract_body e2))
  | DOUBLECOLON (e1, e2) -> (l, DOUBLECOLON (extract_body e1, extract_body e2))
  | STRCON (e1, e2) -> (l, STRCON (extract_body e1, extract_body e2))
  | EApp (e1, e2) -> (l, EApp (extract_body e1, extract_body e2))
  | ELet (f, is_rec, args, typ, e1, e2) ->
    if (args <> [] || is_fun typ) then
      extract_body e2
    else 
      (l, ELet (f, is_rec, args, typ, extract_body e1, extract_body e2))
  | EBlock (is_rec, ds, e) ->
    let ds = List.fold_left (fun ds (f, is_rec, args, typ, e) ->
      if (args <> [] || is_fun typ) then
        ds
      else 
        (f, is_rec, args, typ, extract_body e)::ds
    ) [] ds in
    if ds = [] then extract_body e else (l, EBlock (is_rec, List.rev ds, extract_body e))
  | EMatch (e, bs) -> (l, EMatch (extract_body e, List.map (fun (p, e) -> (p, extract_body e)) bs))
  | IF (e1, e2, e3) -> (l, IF (extract_body e1, extract_body e2, extract_body e3))
  | _ -> raise (Failure ("Call graph-exttract : invalid exp (" ^ exp_to_string (l, exp)))
  
let rec exp_to_cg : env -> func_id -> node * edge * ctx -> lexp -> node * edge
= fun env id (node, edge, ctx) (l, exp) ->
  match exp with 
  | EUnit | Const _ | TRUE | FALSE | String _ | Raise _ -> (node, edge)
  | EVar x -> 
    (* if a variable x is a pre-defined function *)
    if BatMap.mem x env then 
      let id' = BatMap.find x env in
      let ctx = List.rev ctx in
      (* make a call relation between (id, id') with an updated context *)
      if BatMap.mem (id, id') edge then
        let new_ctx = BatSet.add (l, ctx) (BatMap.find (id, id') edge) in
        (node, BatMap.add (id, id') new_ctx edge)
      else (node, BatMap.add (id, id') (BatSet.singleton (l, ctx)) edge)  
    else (node, edge)
  | EList es | ECtor (_, es) | ETuple es -> List.fold_left (fun (node, edge) e -> exp_to_cg env id (node, edge, ctx) e) (node, edge) es
  | EFun (arg, e) -> 
    let new_env = kill_env_by_arg arg env in
    exp_to_cg new_env id (node, edge, ctx) e
  | MINUS e | NOT e -> exp_to_cg env id (node, edge, ctx) e
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) 
  | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LARGER (e1, e2) | LESSEQ (e1, e2) | LARGEREQ (e1, e2) 
  | EQUAL (e1, e2) | NOTEQ (e1, e2) | AT (e1, e2) | DOUBLECOLON (e1, e2) | STRCON (e1, e2) | EApp (e1, e2) ->
    let (node, edge) = exp_to_cg env id (node, edge, ctx) e1 in
    exp_to_cg env id (node, edge, ctx) e2
  | ELet (f, is_rec, args, typ, e1, e2) ->
    (* Kill environment *)
    let func_env = List.fold_left (fun env arg -> kill_env_by_arg arg env) env args in
    if (args <> [] || is_fun typ) then
      (* f is a function *)    
      begin match f with
      | BindOne f -> 
        let new_id = fresh_id () in
        let func_env = if is_rec then BatMap.add f new_id func_env else func_env in
        let new_node = (new_id, extract_input_args (get_intput_args args) e1, get_output_typ typ, extract_body e1) in
        let node = if is_rec then BatMap.add f new_node node else node in (* ??? *)
        let (node, edge) = exp_to_cg func_env new_id (node, edge, []) e1 in (* Init ctx and analize body expression *)
        exp_to_cg (BatMap.add f new_id env) id (BatMap.add f new_node node, edge, ctx) e2
      | _ -> raise (Failure "Call graph : invalid function definition")
    end
    else 
      let (node, edge) = exp_to_cg func_env id (node, edge, ctx) e1 in
      exp_to_cg (kill_env_by_binding f env) id (node, edge, ctx) e2
  | EBlock (is_rec, ds, e) ->
    (* Init env *)
    let (env, node) = List.fold_left (fun (env, node) (f, is_rec, args, typ, e) -> 
      if (args <> [] || is_fun typ) then 
        begin match f with
        | BindOne f -> 
          let new_id = fresh_id () in
          let new_node = (new_id, extract_input_args (get_intput_args args) e, get_output_typ typ, extract_body e) in
          (BatMap.add f new_id env, BatMap.add f new_node node)
        | _ -> raise (Failure "Call graph : invalid function definition")
        end
      else 
        (kill_env_by_binding f env, node)
    ) (env, node) ds in
    (* Evaluate all bindings *)
    let (node, edge) = List.fold_left (fun (node, edge) (f, is_rec, args, typ, e) -> 
      if (args <> [] || is_fun typ) then 
        begin match f with
        | BindOne f -> 
          (* If f is a recursive function, kill f *)
          (try
            let new_id = BatMap.find f env in
            let func_env = if is_rec then env else BatMap.remove f env in
            let node = if is_rec then node else BatMap.remove f node in (* ??? *)
            exp_to_cg func_env new_id (node, edge, []) e
          with _ -> failwith "Something are wrong...")
        | _ -> raise (Failure "Call graph : invalid function definition")
        end
      else 
        exp_to_cg env id (node, edge, ctx) e
    ) (node, edge) ds in
    exp_to_cg env id (node, edge, ctx) e
  | EMatch (e, bs) ->
    let (node, edge) = exp_to_cg env id (node, edge, ctx) e in
    let (node, edge, _) = List.fold_left (fun (node, edge, ctx) (p, e') ->  
      let env = kill_env_by_pat p env in
      let new_ctx = ((gen_label (), EQUAL (pat_to_exp p, e)))::ctx in
      let (node, edge) = exp_to_cg env id (node, edge, new_ctx) e' in
      (node, edge, ((gen_label (), NOTEQ (pat_to_exp p, e))::ctx))
    ) (node, edge, ctx) (flatten_branch bs) in
    (node, edge)
  | IF (e1, e2, e3) ->
    let (node, edge) = exp_to_cg env id (node, edge, ctx) e1 in
    let (node, edge) = exp_to_cg env id (node, edge, (e1::ctx)) e2 in
    exp_to_cg env id (node, edge, ((gen_label (), NOT e1)::ctx)) e3
  | _ -> raise (Failure ("Call graph : invalid exp (" ^ exp_to_string (l, exp)))

let rec decl_to_cg : env -> node * edge -> decl -> env * node * edge
= fun env (node, edge) decl ->
  match decl with
  | DLet (f, is_rec, args, typ, e) -> 
    (* Kill environment *)
    let func_env = List.fold_left (fun env arg -> kill_env_by_arg arg env) env args in
    if (args <> [] || is_fun typ) then
      (* f is a function *)    
      begin match f with
      | BindOne f -> 
        let new_id = fresh_id () in
        let func_env = if is_rec then BatMap.add f new_id func_env else func_env in
        let new_node = (new_id, extract_input_args (get_intput_args args) e, get_output_typ typ, extract_body e) in
        let node = if is_rec then BatMap.add f new_node node else node in (* ??? *)
        let (node, edge) = exp_to_cg func_env new_id (node, edge, []) e in (* Init ctx *)
        (BatMap.add f new_id env, BatMap.add f new_node node, edge)
      | _ -> raise (Failure "Call graph : invalid function definition")
      end
    else 
      (env, node, edge)
  | DBlock (is_rec, ds) -> 
    (* Init env *)
    let (env, node) = List.fold_left (fun (env, node) (f, is_rec, args, typ, e) -> 
      if (args <> [] || is_fun typ) then 
        begin match f with
        | BindOne f -> 
          let new_id = fresh_id () in
          let new_node = (new_id, extract_input_args (get_intput_args args) e, get_output_typ typ, extract_body e) in
          (BatMap.add f new_id env, BatMap.add f new_node node)
        | _ -> raise (Failure "Call graph : invalid function definition")
        end
      else 
        (kill_env_by_binding f env, node)
    ) (env, node) ds in
    (* Evaluate all bindings *)
    let (node, edge) = List.fold_left (fun (node, edge) (f, is_rec, args, typ, e) -> 
      if (args <> [] || is_fun typ) then 
        begin match f with
        | BindOne f -> 
          (* If f is a recursive function, kill f *)
          let new_id = BatMap.find f env in
          let func_env = if is_rec then env else BatMap.remove f env in
          let node = if is_rec then node else BatMap.remove f node in (* ??? *)
          exp_to_cg func_env new_id (node, edge, []) e
        | _ -> raise (Failure "Call graph : invalid function definition")
        end
      else 
        (node, edge)
    ) (node, edge) ds in
    (env, node, edge)
  | _ -> (env, node, edge)

(* Remove testing function from call-graph *)
(*
let remove_grading : graph -> graph -> graph
= fun g_grading g_original ->
  let names = BatMap.foldi (fun name (_, _, _) acc -> BatSet.add name acc) (get_node g_original) BatSet.empty in
  let original_functions = BatMap.foldi (fun name (id, _, _) acc -> 
    if BatSet.mem name names then BatMap.add name id acc else acc
  ) (get_node g_grading) BatMap.empty in
  g_original
*)

(* Remove unreachable functions => to use library codes *)


(* Extract call graph of a given program *)
let run : prog -> graph
= fun pgm -> 
  let pgm_with_grading = 
    pgm@(!grading_pgm) 
    |> preprocess
    |> T.run
  in
  let pgm = 
    pgm@(!grading_pgm) 
    |> T.run
    |> preprocess
  in
  let (env, node, edge) = List.fold_left (fun (env, node, edge) decl -> decl_to_cg env (node, edge) decl) (BatMap.empty, BatMap.empty, BatMap.empty) pgm_with_grading in
  let (env, node, edge) = List.fold_left (fun (env, node, edge) decl -> decl_to_cg env (node, edge) decl) (BatMap.empty, BatMap.empty, BatMap.empty) pgm in
  (node, edge, BatMap.find !Options.opt_entry_func env)