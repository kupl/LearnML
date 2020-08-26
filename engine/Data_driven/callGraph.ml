open Lang
open Util
open Print 
open Type 

exception UndefinedNode

(* Call-graph node *)
type node_id = int
type node = {
  id : node_id;
  name : string;
  args : arg list; 
  typ : typ;
  body : lexp;
}

(* Call-graph edge : call relation between two function (except recursion) *)
type edge = {
  src : node_id;
  sink : node_id;
  ctx : path;
}
and path =
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
and op = Add | Sub | Mul | Div | Mod
and comb = And | Or
and comp = Lt | Gt | Le | Ge
and eq = Eq | NEq

type graph = { nodes : node BatSet.t; edges : edge BatSet.t }

(* To string *)
let string_of_node node =
  "-------------(" ^ string_of_int node.id ^ ")-------------\n" ^
  node.name ^ (args_to_string node.args "") ^ " : " ^ type_to_string node.typ ^ "\n" ^
  "---------------------------\n" ^
  exp_to_string node.body

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

let string_of_edge edge = 
  string_of_int edge.src ^ "->" ^ string_of_int edge.sink ^ " : " ^ string_of_path edge.ctx

let string_of_graph cg =
  "Nodes : \n" ^ string_of_set ~first:"" ~last:"" ~sep:"\n" string_of_node cg.nodes ^ 
  "\nEdge : \n" ^ string_of_set ~first:"" ~last:"" ~sep:"\n" string_of_edge cg.edges

let print_graph cg = print_endline (string_of_graph cg)

(* Utility functions *)
let h_t = ref BatMap.empty 
let v_t = ref BatMap.empty 

let id_num = ref 0
let fresh_id () = id_num := !id_num + 1; !id_num

let symbol_num = ref 0
let fresh_symbol () = symbol_num := !symbol_num + 1; !symbol_num

let get_node_by_id : node_id -> graph -> node
= fun id cg -> try List.find (fun node -> id = node.id) (BatSet.to_list cg.nodes) with Not_found -> raise UndefinedNode

let get_node_by_name : id -> graph -> node
= fun name cg -> try List.find (fun node -> name = node.name) (BatSet.to_list cg.nodes) with Not_found -> raise UndefinedNode

let update_node : node -> graph -> graph
= fun node cg -> { nodes = BatSet.add node cg.nodes; edges = cg.edges }

let update_edge : edge -> graph -> graph
= fun edge cg -> { nodes = cg.nodes; edges = BatSet.add edge cg.edges }

(* Path encoding *)
let fresh_path = Bool true

let rec update_path : path -> path -> path
= fun new_path path -> Bop (And, path, new_path)

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
  | PCons (phd, ptl) ->
    begin match typ with
    | TList t -> Concat (pat_to_path l phd t, pat_to_path l ptl (TList t))
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

let rec arg_to_path : arg -> path
= fun arg ->
  match arg with
  | ArgUnder typ -> Symbol (fresh_symbol (), typ)
  | ArgOne (x, typ) -> Var (x, typ)
  | ArgTuple args -> Tuple (List.map arg_to_path args)

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
let rec extract_input_args : arg list -> lexp -> arg list
= fun args (l, exp) ->
  match exp with
  | EFun (arg, e) -> extract_input_args (args@[arg]) e
  | ELet (_, _, _, _, _, e) | EBlock (_, _, e) -> extract_input_args args e
  | _ -> args

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
  | _ -> raise (Failure ("Call graph : invalid exp (" ^ exp_to_string (l, exp)))

let rec exp_to_cg : graph -> node_id -> path -> lexp -> graph
= fun cg id path (l, exp) ->
  match exp with 
  | EUnit | Const _ | TRUE | FALSE | String _ -> cg
  | EVar x -> 
    (try
      let node' = get_node_by_name x cg in
      let new_edge = { src = id; sink = node'.id; ctx = path } in
      update_edge new_edge cg
    with UndefinedNode -> cg)
  | EList es | ECtor (_, es) | ETuple es -> List.fold_left (fun cg e -> exp_to_cg cg id path e) cg es
  | EFun (_, e) | MINUS e | NOT e | Raise e | ERef e | EDref e -> exp_to_cg cg id path e
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) 
  | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LARGER (e1, e2) | LESSEQ (e1, e2) | LARGEREQ (e1, e2) 
  | EQUAL (e1, e2) | NOTEQ (e1, e2) | AT (e1, e2) | DOUBLECOLON (e1, e2) | STRCON (e1, e2) | EApp (e1, e2) 
  | EAssign (e1, e2) -> 
    let cg1 = exp_to_cg cg id path e1 in
    exp_to_cg cg1 id path e2
  | ELet (f, is_rec, args, typ, e1, e2) ->
    if (args <> [] || is_fun typ) then
      (* If f is a function then generate new call-graph node *)    
      begin match f with
      | BindOne f -> 
        let new_node = { id = fresh_id (); name = f; args = args; typ = get_func_typ args typ; body = extract_body e1 } in 
        let cg1 = exp_to_cg cg new_node.id fresh_path e1 in 
        exp_to_cg (update_node new_node cg1) id path e2
      | _ -> raise (Failure "Call graph : invalid function definition")
      end
    else 
      let cg1 = exp_to_cg cg id path e1 in
      let new_path = EQop (Eq, binding_to_path f typ, exp_to_path (extract_body e1))  in
      exp_to_cg cg1 id (update_path new_path path) e2
  | EBlock (is_rec, ds, e) ->
    (* Define call graph nodes *)
    let cg = List.fold_left (fun acc (f, is_rec, args, typ, e) -> 
      if (args <> [] || is_fun typ) then
        begin match f with
        | BindOne f -> 
          let new_node = { id = fresh_id (); name = f; args = args; typ = get_func_typ args typ; body = extract_body e } in 
          update_node new_node acc
        | _ -> raise (Failure "Call graph : invalid function definition")
        end
      else acc
    ) cg ds in
    (* Draw edges between predefined nodes *)
    let (cg, path) = List.fold_left (fun (cg, path) (f, is_rec, args, typ, e) ->
      if (args <> [] || is_fun typ) then
        begin match f with
        | BindOne f -> (try
            let current_node = get_node_by_name f cg in
            (exp_to_cg cg current_node.id fresh_path e, path)
          with UndefinedNode -> raise (Failure "Call graph : fail to init mutually defined functions"))
        | _ -> raise (Failure "Call graph : invalid function definition")
        end
      else  
        let new_path = EQop (Eq, binding_to_path f typ, exp_to_path (extract_body e))  in
        (exp_to_cg cg id path e, (update_path new_path path))
    ) (cg, path) ds in
    exp_to_cg cg id path e
  | EMatch (e, bs) ->
    let cg = exp_to_cg cg id path e in
    let path_e = exp_to_path e in
    let (cg, _) = List.fold_left (fun (cg, path) (p, e') ->  
      let path_p = pat_to_path (get_label e') p (BatMap.find (get_label e) !h_t) in
      let cg = exp_to_cg cg id (update_path (EQop (Eq, path_e, path_p)) path) e' in
      (cg, update_path (EQop (NEq, path_e, path_p)) path)
    ) (cg, path) bs in
    cg
  | IF (e1, e2, e3) ->
    let new_path = exp_to_path e1 in
    let cg1 = exp_to_cg cg id path e1 in
    let cg2 = exp_to_cg cg1 id (update_path new_path path) e2 in
    exp_to_cg cg2 id (update_path (Not new_path) path) e3
  | _ -> raise (Failure ("Call graph : invalid exp (" ^ exp_to_string (l, exp)))

let rec decl_to_cg : graph -> decl -> graph
= fun cg decl ->
  match decl with
  | DLet (f, is_rec, args, typ, e) -> 
    if (args <> [] || is_fun typ) then  
      begin match f with
      | BindOne f -> 
        let new_node = { id = fresh_id (); name = f; args = args; typ = get_func_typ args typ; body = extract_body e } in 
        update_node new_node (exp_to_cg cg new_node.id fresh_path e)
      | _ -> raise (Failure "Call graph : invalid function definition")
      end
    else cg
  | DBlock (is_rec, ds) -> 
    let cg = List.fold_left (fun acc (f, is_rec, args, typ, e) -> 
      if (args <> [] || is_fun typ) then
        begin match f with
        | BindOne f -> 
          let new_node = { id = fresh_id (); name = f; args = args; typ = get_func_typ args typ; body = extract_body e } in 
          update_node new_node acc
        | _ -> raise (Failure "Call graph : invalid function definition")
        end
      else acc
    ) cg ds in
    List.fold_left (fun cg (f, is_rec, args, typ, e) ->
      if (args <> [] || is_fun typ) then
        begin match f with
        | BindOne f -> (try
            let current_node = get_node_by_name f cg in
            exp_to_cg cg current_node.id fresh_path e
          with UndefinedNode -> raise (Failure "Call graph : fail to init mutually defined functions"))
        | _ -> raise (Failure "Call graph : invalid function definition")
        end
      else cg
    ) cg ds
  | _ -> cg

(* Extract call graph of a given program *)
let extract_graph : prog -> graph
= fun pgm -> 
  (* Update type inforation for path encoding *)
  let (_, h_t', v_t', _) = Type2.run pgm in
  h_t := h_t';
  v_t := v_t';
  let cg = List.fold_left (fun cg decl -> decl_to_cg cg decl) { nodes = BatSet.empty; edges = BatSet.empty } pgm in
  cg