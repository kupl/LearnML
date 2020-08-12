open Lang
open Util
open Print 
open Type 

(***************************)
(* Call_graph construction *)
(***************************)
exception PathError
(* Assume that all identifiers are uniquely defined => kill is needless *)
(* Node *)
type func_id = int
type nodes = (id, (func_id * arg list * typ * lexp)) BatMap.t (* function name -> (parameter info, output type, body) *)
(* Edge *)
type op = Add | Sub | Mul | Div | Mod
type comb = And | Or
type comp = Lt | Le | Gt | Ge 
type eq = Eq | NEq 
type path =
  | Unit
  | Int of int
  | Bool of bool
  | Str of string
  | Aop of op * path * path
  | Bop of comb * path  * path
  | ABop of comp * path * path
  | EQop of eq * path * path
  | Strcon of path * path
  | Append of path * path
  | Concat of path * path
  | Minus of path
  | Not of path 
  | App of path * path
  | Tuple of path list
  | Ctor of id * path list 
  | List of path list * typ 
  | Var of id * typ (* Symbol with name *)
  | Symbol of int * typ (* unencodable path condition *)
type edges = (func_id * func_id, ctx) BatMap.t (* (caller, callee) -> ctx *)
and ctx = (label * path) BatSet.t (* ctx -> set of label * path *)
(* G = (V, E, entry) *)
type graph = nodes * edges * func_id

type env = (id, func_id) BatMap.t (* Var -> func_id *)

(* Utility functions *)
let h_t = ref BatMap.empty 
let v_t = ref BatMap.empty 

let id_num = ref 0
let fresh_id () = id_num := !id_num + 1; !id_num

let symbol_num = ref 0
let fresh_symbol () = symbol_num := !symbol_num + 1; !symbol_num
  
let get_nodes : graph -> nodes
= fun (nodes, edges, entry) -> nodes

let get_edges : graph -> edges
= fun (nodes, edges, entry) -> edges 

let get_entry : graph -> func_id
= fun (nodes, edges, entry) -> entry 

let rec get_function_name : func_id -> graph -> id
= fun id (nodes, edges, entry) -> let (name, _) = List.find (fun (name, (id', _, _, _)) -> id = id') (BatMap.bindings nodes) in name

let rec get_reachable : func_id -> graph -> func_id BatSet.t
= fun id (nodes, edges, entry) ->
  let rec iter reachable = 
    let reachable' = BatSet.fold (fun id acc -> 
      BatMap.foldi (fun (s, t) _ acc -> if s = id then BatSet.add t acc else acc) edges acc
    ) reachable reachable in
    if BatSet.equal reachable reachable' then reachable' else iter reachable'
  in
  iter (BatSet.singleton id)

let rec get_caller : func_id -> graph -> func_id BatSet.t
= fun id (nodes, edges, entry) -> BatMap.foldi (fun (s, t) _ acc -> if t = id then BatSet.add s acc else acc) edges BatSet.empty

let rec get_entry_name : graph -> id
= fun graph -> get_function_name (get_entry graph) graph

let rec get_caller_by_name : id -> graph -> id BatSet.t
= fun id graph ->
  let (func_id, _, _, _) = BatMap.find id (get_nodes graph) in 
  BatSet.map (fun func_id -> get_function_name func_id graph) (get_caller func_id graph)

let rec get_linked_edges : func_id -> graph -> edges 
= fun id graph -> 
  BatMap.filter (fun (s, t) ctx -> id = t && s <> t) (get_edges graph)

let rec get_invoked_path : func_id -> graph -> path BatSet.t
= fun id graph -> 
  BatMap.foldi (fun (s, t) ctx acc -> 
    if id = t && s <> t then BatSet.union acc (BatSet.map (fun (label, path) -> path) ctx) else acc
  ) (get_edges graph) BatSet.empty

(* To string *)
let string_of_nodes : nodes -> string
= fun nodes ->  
  BatMap.foldi (fun f (id, args, typ, body) acc ->
    let temp_oc = open_out "temp.ml" in
    let exp_str = Print.exp_to_string body in
    let str = 
      "---------------------------\n" ^
      f ^ " : " ^ type_to_string typ ^ " " ^ (args_to_string args "") ^ "\n" ^
      "---------------------------\n" ^
      Print.exp_to_string body
    in
    if acc = "" then str else acc ^ "\n" ^ str
  ) nodes ""

let rec string_of_path path = 
  let string_of_op op =
    match op with
    | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%"
  in
  let string_of_comb comb =
    match comb with
    | And -> "/\\" | Or -> "\\/"
  in
  let string_of_comp comp =
    match comp with
    | Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">="
  in
  let string_of_eq eq =
    match eq with
    | Eq -> "=" | NEq -> "<>"
  in
  match path with
  | Unit -> "()"
  | Int n -> string_of_int n
  | Bool b -> if b then "true" else "false"
  | Str str -> str
  | Aop (op, p1, p2) -> string_of_path p1 ^ string_of_op op ^ string_of_path p2
  | Bop (comb, p1, p2) -> string_of_path p1 ^ string_of_comb comb ^ string_of_path p2
  | ABop (comp, p1, p2) -> string_of_path p1 ^ string_of_comp comp ^ string_of_path p2
  | EQop (eq, p1, p2) -> string_of_path p1 ^ string_of_eq eq ^ string_of_path p2
  | Strcon (p1, p2) -> string_of_path p1 ^ "^" ^ string_of_path p2
  | Append (p1, p2) -> string_of_path p1 ^ "@" ^ string_of_path p2
  | Concat (p1, p2) -> string_of_path p1 ^ "::" ^ string_of_path p2
  | Minus p -> "-(" ^ string_of_path p ^ ")" 
  | Not p -> "not (" ^ string_of_path p ^ ")" 
  | App (p1, p2) -> "(" ^ string_of_path p1 ^ " " ^ string_of_path p2 ^ ")"
  | List (ps, typ) -> pp_list string_of_path ps ^ ":" ^ type_to_string typ
  | Tuple ps -> pp_tuple string_of_path ps
  | Ctor (c, ps) -> c ^ (if ps = [] then "" else " (" ^ string_of_path (List.hd ps) ^ ")") 
  | Var (x, typ) -> x ^ ":" ^ type_to_string typ
  | Symbol (n, typ) -> "?(" ^ string_of_int n ^ ")" ^ ":" ^ type_to_string typ

let rec string_of_ctxs : ctx -> string
= fun ctxs ->
  BatSet.fold (fun (l, path) acc ->
    if acc = "" then
      string_of_int l ^ " : " ^ string_of_path path
    else 
      acc ^ "\n" ^ string_of_int l ^ " : " ^ string_of_path path
  ) ctxs ""

let string_of_edges : edges -> string
= fun edges -> 
  string_of_map (fun (s, t) -> "(" ^ string_of_int s ^ ", " ^ string_of_int t ^ ")") 
                string_of_ctxs edges

let string_of_graph : graph -> string
= fun (nodes, edges, entry) ->
  "Node : \n" ^ string_of_nodes nodes ^
  "\nEdge : \n" ^ string_of_edges edges 

let print_graph : graph -> unit
= fun cg -> print_endline (string_of_graph cg)

(* Input, output type *)
let rec get_input_args : arg list -> arg list
= fun args -> args

let rec extract_input_args : arg list -> lexp -> arg list
= fun args (l, exp) ->
  match exp with
  | EFun (arg, e) -> extract_input_args (args@[arg]) e
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
  | PList ps | PCons ps | PTuple ps | PCtor (_, ps) | Pats ps -> List.fold_left (fun env p -> kill_env_by_pat p env) env ps

(* Path encoding *)
let rec update_path : path -> path -> path
= fun new_path path -> Bop (And, path, new_path)

let rec args_to_path : arg list -> path 
= fun args -> 
  (*
  let rec flatten_args : arg list -> arg list 
  = fun args ->
    match args with
    | [] -> []
    | (ArgTuple args)::tl -> (flatten_args args)@(flatten_args tl)
    | arg::tl -> arg::(flatten_args tl)
  in
  let paths = List.fold_left (fun acc arg ->
    let new_path =
      match arg with
      | ArgUnder typ -> EQop (Eq, Var ("#arg" ^ string_of_int (List.length acc), typ), Symbol (fresh_symbol (), typ))
      | ArgOne (x, typ) -> EQop (Eq, Var ("#arg" ^ string_of_int (List.length acc), typ), Var (x, typ))
      | _ -> raise (Failure ("Call grpah: invalid arg (" ^ arg_to_string arg ^ ") while encoding  "))
    in 
    new_path::acc
  ) [] (flatten_args args) in
  let paths = List.rev paths in
  List.fold_left (fun acc path -> update_path path acc) (List.hd paths) (List.tl paths)
  *)
  Bool true

let rec pat_to_path : label -> pat -> typ -> path
= fun l p typ ->
  match p with
  | PUnit -> Unit
  | PInt n -> Int n
  | PBool b -> Bool b 
  | PVar x -> Var (x, BatMap.find x (BatMap.find l !v_t))
  | PUnder -> Symbol (fresh_symbol (), typ)
  | PList ps -> 
    begin match typ with
    | TList t -> List (List.map (fun p -> pat_to_path l p t) ps, TList t)
    | _ -> raise (Failure ("Call grpah: invalid pat (" ^ pat_to_string p ^ ") while encoding path"))
    end
  | PCons [p]-> pat_to_path l p typ
  | PCons (phd::ptl) ->
    begin match typ with
    | TList t -> Concat (pat_to_path l phd t, pat_to_path l (PCons ptl) (TList t))
    | _ -> raise (Failure ("Call grpah: invalid pat (" ^ pat_to_string p ^ ") while encoding path"))
    end
  | PTuple ps -> 
    begin match typ with
    | TTuple ts -> Tuple (List.map2 (fun p t -> pat_to_path l p t) ps ts)
    | _ -> raise (Failure ("Call grpah: invalid pat (" ^ pat_to_string p ^ ") while encoding path"))
    end
  | PCtor (c, ps) -> 
    begin match BatMap.find c (BatMap.find l !v_t) with 
    | TCtor (tname, ts) -> Ctor (c, List.map2 (fun p t -> pat_to_path l p t) ps ts)
    | _ -> raise (Failure ("Call grpah: invalid pat (" ^ pat_to_string p ^ ") while encoding path"))
    end
  | _ -> raise (Failure ("Call grpah: invalid pat (" ^ pat_to_string p ^ ") while encoding path"))

let rec binding_to_path : let_bind -> typ -> path 
= fun binding typ ->
  match binding with
  | BindOne x -> Var (x, typ)
  | BindTuple bs -> 
    begin match typ with
    | TTuple ts when List.length bs = List.length ts -> Tuple (List.map2 (fun b typ -> binding_to_path b typ) bs ts)
    | _ -> raise (Failure ("Call grpah: invalid binding (" ^ let_to_string binding ^ ") while encoding path"))
    end
  | _ -> Symbol (fresh_symbol (), typ)

let rec exp_to_path : lexp -> path
= fun (l, exp) ->
  match exp with 
  | EUnit -> Unit
  | Const n -> Int n
  | TRUE -> Bool true 
  | FALSE -> Bool false 
  | String str -> Str str 
  | EVar x -> Var (x, BatMap.find x (BatMap.find l !v_t))
  | EList es -> List (List.map exp_to_path es, BatMap.find l !h_t)
  | ETuple es -> Tuple (List.map exp_to_path es)
  | ECtor (c, es) -> Ctor (c, List.map exp_to_path es)
  | MINUS e -> Minus (exp_to_path e)
  | NOT e -> Not (exp_to_path e)
  | ADD (e1, e2) -> Aop (Add, exp_to_path e1, exp_to_path e2)
  | SUB (e1, e2) -> Aop (Sub, exp_to_path e1, exp_to_path e2)
  | MUL (e1, e2) -> Aop (Mul, exp_to_path e1, exp_to_path e2)
  | DIV (e1, e2) -> Aop (Div, exp_to_path e1, exp_to_path e2)
  | MOD (e1, e2) -> Aop (Mod, exp_to_path e1, exp_to_path e2)
  | OR (e1, e2) -> Bop (Or, exp_to_path e1, exp_to_path e2)
  | AND (e1, e2) -> Bop (And, exp_to_path e1, exp_to_path e2)
  | LESS (e1, e2) -> ABop (Lt, exp_to_path e1, exp_to_path e2)
  | LARGER (e1, e2) -> ABop (Gt, exp_to_path e1, exp_to_path e2)
  | LESSEQ (e1, e2) -> ABop (Le, exp_to_path e1, exp_to_path e2)
  | LARGEREQ (e1, e2) -> ABop (Ge, exp_to_path e1, exp_to_path e2)
  | EQUAL (e1, e2) -> EQop (Eq, exp_to_path e1, exp_to_path e2)
  | NOTEQ (e1, e2) -> EQop (NEq, exp_to_path e1, exp_to_path e2)
  | AT (e1, e2) -> Append (exp_to_path e1, exp_to_path e2)
  | DOUBLECOLON (e1, e2) -> Concat (exp_to_path e1, exp_to_path e2)
  | STRCON (e1, e2) -> Strcon (exp_to_path e1, exp_to_path e2)
  | EApp (e1, e2) -> (* App (exp_to_path e1, exp_to_path e2) *) Symbol (fresh_symbol (), BatMap.find l !h_t)
  | EAssign _ | Raise _ | ERef _ | EDref _ | EFun _ | ELet _ | EBlock _ | EMatch _ | IF _ -> Symbol (fresh_symbol (), BatMap.find l !h_t)
  | _ -> raise (Failure ("Call graph : invalid exp (" ^ exp_to_string (l, exp) ^ ") while encoding path"))

(* Extract functions body by unrolling the nested function definition *)
let rec extract_body : lexp -> lexp
= fun (l, exp) ->
  match exp with 
  | EUnit | Const _ | TRUE | FALSE | String _ | EVar _ -> (l, exp)
  | EList es -> (l, EList (List.map (fun e -> extract_body e) es))
  | ECtor (x, es) -> (l, ECtor (x, List.map (fun e -> extract_body e) es))
  | ETuple es -> (l, ETuple (List.map (fun e -> extract_body e) es))
  | EFun (arg, e) -> (l, EFun (arg, extract_body e))
  | MINUS e | NOT e | Raise e | ERef e | EDref e -> (l, update_unary exp (extract_body e))
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) 
  | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LARGER (e1, e2) | LESSEQ (e1, e2) 
  | LARGEREQ (e1, e2) | EQUAL (e1, e2) | NOTEQ (e1, e2) | AT (e1, e2) | DOUBLECOLON (e1, e2) 
  | STRCON (e1, e2) | EApp (e1, e2) | EAssign (e1, e2) -> (l, update_binary exp (extract_body e1, extract_body e2))
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
  
let rec exp_to_cg : env -> func_id -> nodes * edges * path -> lexp -> nodes * edges
= fun env id (nodes, edges, path) (l, exp) ->
  (* let _ = print_endline (lexp_to_string (l, exp)) in *)
  match exp with 
  | EUnit | Const _ | TRUE | FALSE | String _ -> (nodes, edges)
  | EVar x -> 
    (* if a variable x is a pre-defined function *)
    if BatMap.mem x env then 
      let id' = BatMap.find x env in
      (* make a call relation between (id, id') with an updated context *)
      if BatMap.mem (id, id') edges then
        let new_ctx = BatSet.add (l, path) (BatMap.find (id, id') edges) in
        (nodes, BatMap.add (id, id') new_ctx edges)
      else (nodes, BatMap.add (id, id') (BatSet.singleton (l, path)) edges)  
    else (nodes, edges)
  | EList es | ECtor (_, es) | ETuple es -> List.fold_left (fun (nodes, edges) e -> exp_to_cg env id (nodes, edges, path) e) (nodes, edges) es
  | EFun (arg, e) -> 
    let new_env = kill_env_by_arg arg env in
    exp_to_cg new_env id (nodes, edges, path) e
  | MINUS e | NOT e | Raise e | ERef e | EDref e -> exp_to_cg env id (nodes, edges, path) e
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) 
  | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LARGER (e1, e2) | LESSEQ (e1, e2) | LARGEREQ (e1, e2) 
  | EQUAL (e1, e2) | NOTEQ (e1, e2) | AT (e1, e2) | DOUBLECOLON (e1, e2) | STRCON (e1, e2) | EApp (e1, e2) 
  | EAssign (e1, e2) ->
    let (nodes, edges) = exp_to_cg env id (nodes, edges, path) e1 in
    exp_to_cg env id (nodes, edges, path) e2
  | ELet (f, is_rec, args, typ, e1, e2) ->
    (* Kill environment *)
    let func_env = List.fold_left (fun env arg -> kill_env_by_arg arg env) env args in
    if (args <> [] || is_fun typ) then
      (* f is a function *)    
      begin match f with
      | BindOne f -> 
        let new_id = fresh_id () in
        let func_env = if is_rec then BatMap.add f new_id func_env else func_env in
        let new_node = (new_id, extract_input_args (get_input_args args) e1, get_output_typ typ, extract_body e1) in
        let nodes = if is_rec then BatMap.add f new_node nodes else nodes in
        let (nodes, edges) = exp_to_cg func_env new_id (nodes, edges, args_to_path args) e1 in (* Init ctx and analize body expression *)
        exp_to_cg (BatMap.add f new_id env) id (BatMap.add f new_node nodes, edges, path) e2
      | _ -> raise (Failure "Call graph : invalid function definition")
    end
    else 
      let (nodes, edges) = exp_to_cg func_env id (nodes, edges, path) e1 in
      let new_path = EQop (Eq, binding_to_path f typ, exp_to_path (extract_body e1))  in
      exp_to_cg (kill_env_by_binding f env) id (nodes, edges, update_path new_path path) e2
  | EBlock (is_rec, ds, e) ->
    (* Init env *)
    let (env, nodes) = List.fold_left (fun (env, nodes) (f, is_rec, args, typ, e) -> 
      if (args <> [] || is_fun typ) then 
        begin match f with
        | BindOne f -> 
          let new_id = fresh_id () in
          let new_node = (new_id, extract_input_args (get_input_args args) e, get_output_typ typ, extract_body e) in
          (BatMap.add f new_id env, BatMap.add f new_node nodes)
        | _ -> raise (Failure "Call graph : invalid function definition")
        end
      else 
        (kill_env_by_binding f env, nodes)
    ) (env, nodes) ds in
    (* Evaluate all bindings *)
    let (nodes, edges, path) = List.fold_left (fun (nodes, edges, path) (f, is_rec, args, typ, e) -> 
      if (args <> [] || is_fun typ) then 
        begin match f with
        | BindOne f -> 
          (* If f is a recursive function, kill f *)
          (try
            let new_id = BatMap.find f env in
            let func_env = if is_rec then env else BatMap.remove f env in
            let nodes = if is_rec then nodes else BatMap.remove f nodes in (* ??? *)
            let (nodes, edges) = exp_to_cg func_env new_id (nodes, edges, args_to_path args) e in
            (nodes, edges, path)
          with _ -> failwith "Something are wrong...")
        | _ -> raise (Failure "Call graph : invalid function definition")
        end
      else
        let new_path = EQop (Eq, binding_to_path f typ, exp_to_path (extract_body e))  in 
        let path = update_path new_path path in
        let (nodes, edges) = exp_to_cg env id (nodes, edges, path) e in
        (nodes, edges, path)
    ) (nodes, edges, path) ds in
    exp_to_cg env id (nodes, edges, path) e
  | EMatch (e, bs) ->
    let (nodes, edges) = exp_to_cg env id (nodes, edges, path) e in
    let path_e = exp_to_path e in
    let (nodes, edges, _) = List.fold_left (fun (nodes, edges, path) (p, e') ->  
      let env = kill_env_by_pat p env in
      let path_p = pat_to_path (get_label e') p (BatMap.find (get_label e) !h_t) in
      let (nodes, edges) = exp_to_cg env id (nodes, edges, update_path (EQop (Eq, path_e, path_p)) path) e' in
      (nodes, edges, update_path (EQop (NEq, path_e, path_p)) path)
    ) (nodes, edges, path) bs in
    (nodes, edges)
  | IF (e1, e2, e3) ->
    let (nodes, edges) = exp_to_cg env id (nodes, edges, path) e1 in
    let new_path = exp_to_path e1 in
    let (nodes, edges) = exp_to_cg env id (nodes, edges, update_path new_path path) e2 in
    exp_to_cg env id (nodes, edges, update_path (Not new_path) path) e3
  | _ -> raise (Failure ("Call graph : invalid exp (" ^ exp_to_string (l, exp)))

let rec decl_to_cg : env -> nodes * edges -> decl -> env * nodes * edges
= fun env (nodes, edges) decl ->
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
        let new_node = (new_id, extract_input_args (get_input_args args) e, get_output_typ typ, extract_body e) in
        let nodes = if is_rec then BatMap.add f new_node nodes else nodes in (* ??? *)
        let (nodes, edges) = exp_to_cg func_env new_id (nodes, edges, args_to_path args) e in (* Init ctx *)
        (BatMap.add f new_id env, BatMap.add f new_node nodes, edges)
      | _ -> raise (Failure "Call graph : invalid function definition")
      end
    else 
      (* Update path by global variable? *)
      (env, nodes, edges)
  | DBlock (is_rec, ds) -> 
    (* Init env *)
    let (env, nodes) = List.fold_left (fun (env, nodes) (f, is_rec, args, typ, e) -> 
      if (args <> [] || is_fun typ) then 
        begin match f with
        | BindOne f -> 
          let new_id = fresh_id () in
          let new_node = (new_id, extract_input_args (get_input_args args) e, get_output_typ typ, extract_body e) in
          (BatMap.add f new_id env, BatMap.add f new_node nodes)
        | _ -> raise (Failure "Call graph : invalid function definition")
        end
      else 
        (kill_env_by_binding f env, nodes)
    ) (env, nodes) ds in
    (* Evaluate all bindings *)
    let (nodes, edges) = List.fold_left (fun (nodes, edges) (f, is_rec, args, typ, e) -> 
      if (args <> [] || is_fun typ) then 
        begin match f with
        | BindOne f -> 
          (* If f is a recursive function, kill f *)
          let new_id = BatMap.find f env in
          let func_env = if is_rec then env else BatMap.remove f env in
          let nodes = if is_rec then nodes else BatMap.remove f nodes in (* ??? *)
          exp_to_cg func_env new_id (nodes, edges, args_to_path args) e
        | _ -> raise (Failure "Call graph : invalid function definition")
        end
      else 
        (nodes, edges)
    ) (nodes, edges) ds in
    (env, nodes, edges)
  | _ -> (env, nodes, edges)

(* Remove unreachable functions from call-graph => to use library codes *)
let remove_unrechable : graph -> graph 
= fun graph ->
  let reachable_set = get_reachable (get_entry graph) graph in
  let nodes = BatMap.filter (fun name (id, _, _, _) -> BatSet.mem id reachable_set) (get_nodes graph) in
  let edges = BatMap.filter (fun (s, t) _ -> BatSet.mem s reachable_set && BatSet.mem t reachable_set) (get_edges graph) in
  (nodes, edges, get_entry graph)

(* Remove testing function & unreachable library codes from call-graph *)
let post_process : graph -> graph -> graph
= fun g_grading g_original ->
  let f_original = BatMap.foldi (fun name (_, _, _, _) acc -> BatSet.add name acc) (get_nodes g_original) BatSet.empty in
  let new_entry = BatMap.foldi (fun name (id, _, _, _) entry -> 
    if BatSet.mem name f_original then
      if BatMap.exists (fun name' (id', _, _, _) -> 
        not (BatSet.mem name' f_original) && BatMap.mem (id', id) (get_edges g_grading)) (get_nodes g_grading) 
      then id else entry
    else entry
  ) (get_nodes g_grading) (-1) in
  if new_entry = -1 then
    remove_unrechable g_grading
  else
    remove_unrechable ((get_nodes g_grading), (get_edges g_grading), new_entry)

let update_type_info : prog -> unit
= fun pgm ->
  let (_, h_t', v_t', _) = Type2.run pgm in
  h_t := h_t';
  v_t := v_t'

let rec remove_passing_edges : graph -> graph
= fun (nodes, edges, entry) -> 
  let edges' = BatMap.foldi (fun (s, t) ctxs acc ->
    let passing_ctxs = BatSet.filter (fun (l, path) -> path = Bool true) ctxs in
    if BatSet.is_empty passing_ctxs then
      BatMap.add (s, t) ctxs acc 
    else
      let linked_edges = get_linked_edges s (nodes, edges, entry) in
      let new_edges = BatMap.foldi (fun (s, _) ctxs acc ->
        BatMap.add (s, t) ctxs acc
      ) linked_edges BatMap.empty in
      let ctxs' = BatSet.diff ctxs passing_ctxs in
      BatMap.union new_edges (BatMap.add (s, t) ctxs' acc) 
  ) edges BatMap.empty in
  if BatMap.equal (fun ctxs ctxs' -> BatSet.equal ctxs ctxs') edges edges' then (nodes, edges, entry) 
  else remove_passing_edges (nodes, edges', entry)

(* Extract call graph of a given program *)
let extract_graph : prog -> graph
= fun pgm -> 
  (*
  let pgm_with_grading = Preprocessor.run ((!library_pgm)@pgm@(!grading_pgm)) in
  let _ = update_type_info pgm_with_grading in 
  let (env, node, edge) = List.fold_left (fun (env, node, edge) decl -> decl_to_cg env (node, edge) decl) (BatMap.empty, BatMap.empty, BatMap.empty) pgm_with_grading in
  *)
  let pgm = pgm in
  let _ = update_type_info pgm in
  let (env', nodes', edges') = List.fold_left (fun (env, nodes, edges) decl -> decl_to_cg env (nodes, edges) decl) (BatMap.empty, BatMap.empty, BatMap.empty) pgm in
  (* let g_grading = (node, edge, BatMap.find !Options.opt_entry_func env) in *)
  let g_original = (nodes', edges', -1) in
  (* post_process g_grading g_original *)
  remove_passing_edges g_original
  