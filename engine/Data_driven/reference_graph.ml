open Lang
open Util

module T = Type_annotate

type graph_id = int 

(* node = (node pattern) *)
type node = label * unode
and unode =
  (* Function entry point for specifying function *)
  | Fun of (id * typ) list * node (* input type * body *)
  (* Leaf graphs *)
  | Unit
  | Int of int
  | Bool of bool
  | Str of string
  | Var of id
  | Error
  (* Combinded graphs *)
  | List of node list
  | Tuple of node list
  | Ctor of id * node list
  | Minus of node 
  | Not of node
  | Aop of op * node * node
  | Bop of comb * node * node
  | ABop of comp * node * node
  | EQop of eqop * node * node
  | App of node * node
  | Cons of node * node
  | StrCons of node * node
  (* Call *)
  | Apply of node * node 
  | Call of graph_id
  (* Value Binding => Is binding included in template? *)
  | Let of let_bind * node * node 
  (* Branching point *)
  | If of node * node * node
  | Match of (ctx * node) list
and op = Add | Sub | Mul | Div | Mod
and comb = Or | And
and comp = Le | Lt | Ge | Gt
and eqop = Eq | NEq
and ctx =
  | Sig of node (* node signature for specifying context *)
  | Neg of ctx

(* Edge : call relation between two graphs *)
type t = (graph_id, node) BatMap.t (* node of each function *)

type rg_env = (id, graph_id) BatMap.t (* Var -> node *)

let id_num = ref 0
let new_id () = id_num := !id_num + 1; !id_num

(* To string *)
let string_of_op op
= match op with
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%"

let string_of_comb comb
= match comb with 
  | And -> "&&" | Or -> "||"

let string_of_comp comp
= match comp with 
  | Lt -> "<" | Gt -> ">" | Le -> "<=" | Ge -> ">="

let string_of_eq eq
= match eq with
  | Eq -> "=" | NEq -> "<>"

let rec string_of_node node
= match node with
  | Fun (id, args, node) -> 
    "-----------" ^ string_of_int id ^ "-----------\n" ^
    Print.string_of_set (fun (x, typ) -> "(" ^ x ^ " : " ^ Print.type_to_string typ ^ ")") args ^ "\n" ^
    "----------- Node -----------\n" ^
    string_of_node node
  | Unit -> "()"
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Str s -> s
  | Var x -> x
  | Error -> "Error"
  | List ns -> Print.pp_list string_of_node ns 
  | Tuple ns -> Print.pp_tuple string_of_node ns 
  | Ctor (name, ns) -> name ^ Print.pp_tuple string_of_node ns 
  | Minus node -> "-" ^ string_of_node node
  | Not node -> "not " ^ string_of_node node
  | Aop (op, n1, n2) -> "(" ^ string_of_node n1 ^ string_of_op op ^ string_of_node n2 ^ ")"
  | Bop (comb, n1, n2) -> "(" ^ string_of_node n1 ^ string_of_comb comb ^ string_of_node n2 ^ ")"
  | ABop (comp, n1, n2) -> "(" ^ string_of_node n1 ^ string_of_comp comp ^ string_of_node n2 ^ ")"
  | EQop (eq, n1, n2) -> "(" ^ string_of_node n1 ^ string_of_eq eq ^ string_of_node n2 ^ ")"
  | App (n1, n2) -> "(" ^ string_of_node n1 ^ "@" ^ string_of_node n2 ^ ")"
  | Cons (n1, n2) -> "(" ^ string_of_node n1 ^ "::" ^ string_of_node n2 ^ ")"
  | StrCons (n1, n2) -> "(" ^ string_of_node n1 ^ "^" ^ string_of_node n2 ^ ")"
  | Apply (n1, n2) -> "(" ^ string_of_node n1 ^ " " ^ string_of_node n2 ^ ")"
  | Call id -> "Call-" ^ string_of_int id 
  | Let (let_bind, n1, n2) -> Print.let_to_string let_bind ^ " = " ^ string_of_node n1 ^ "\n" ^ string_of_node n2
  | Branch bs -> "[" ^ (List.fold_left (fun acc (ctx, n) -> acc ^ "\n" ^ string_of_ctx ctx ^ " -> " ^ string_of_node n) "" bs) ^ "]"

and string_of_ctx t
= match t with
  | Sig n -> string_of_node n
  | Neg ctx -> "~ (" ^ string_of_ctx ctx ^ ")"

let print t = 
  BatSet.iter (fun g ->
    print_endline ("{\n" ^ string_of_node g ^ "\n}")
  ) t
  
(* rg extraction *)
let rec is_fun : typ -> bool
= fun typ ->
  match typ with
  | TArr _ -> true
  | _ -> false

(* Environment *)
let kill_env x env = BatMap.remove x env

let rec kill_env_by_arg : arg -> rg_env -> rg_env
= fun arg env ->
  match arg with
  | ArgOne (x, typ) -> kill_env x env
  | ArgTuple xs -> List.fold_left (fun env arg -> kill_env_by_arg arg env) env xs
  | _ -> env

let rec kill_env_by_binding : let_bind -> rg_env -> rg_env
= fun binding env ->
  match binding with
  | BindOne x -> kill_env x env
  | BindTuple bs -> List.fold_left (fun env binding -> kill_env_by_binding binding env) env bs
  | _ -> env

let gen_env (x, id) env = BatMap.add x id env

let gen_env_by_binding : (let_bind * graph_id) -> rg_env -> rg_env
= fun (binding, id) env ->
  match binding with
  | BindOne x -> gen_env (x, id) env
  | _ -> raise (Failure "Invalid function binding")

let find_by_binding : let_bind -> rg_env -> graph_id
= fun binding env ->
  match binding with
  | BindOne x -> BatMap.find x env
  | _ -> raise (Failure "Invalid function binding")

(* Function *)
let rec update_arg : (id * typ) BatSet.t -> arg -> (id * typ) BatSet.t
= fun args arg ->
  match arg with
  | ArgOne (x, typ) -> BatSet.add (x, typ) args
  | ArgTuple xs -> update_args args xs
  | _ -> args 

and update_args : (id * typ) BatSet.t -> arg list -> (id * typ) BatSet.t
= fun args xs -> List.fold_left (fun args arg -> update_arg args arg) args xs

(* Bindings *)
let rec bindings_to_exp : binding list -> lexp -> lexp
= fun bindings e2 ->
  match bindings with
  | [] -> e2
  | (f, is_rec, args, typ, e)::bs -> (gen_label (), ELet (f, is_rec, args, typ, e, bindings_to_exp bs e2))

(* Pattern match *)
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
    
let rec pat_to_node : pat -> node
= fun p ->  
  match p with
  | PUnit -> Unit
  | PInt n -> Int n
  | PBool b -> Bool b
  | PVar x -> Var x
  | PList ps -> List (List.map pat_to_node ps)
  | PCons [p] -> pat_to_node p
  | PCons (phd::ptl) -> Cons (pat_to_node phd, pat_to_node (PCons ptl))
  | PTuple ps -> Tuple (List.map pat_to_node ps)
  | PCtor (x, ps) -> Ctor (x, List.map pat_to_node ps)
  | _ -> raise (Failure "Invalid patten to node")

let rec exp_to_rg : rg_env -> t -> lexp -> (node * t)
= fun env t (l, exp) ->
  let (unode, t) = 
    match exp with 
    | EUnit -> (Unit, t)
    | Const n -> (Int n, t)
    | TRUE -> (Bool true, t)
    | FALSE -> (Bool false, t)
    | String s -> (Str s, t)
    | EVar x -> if BatMap.mem x env then (Call (BatMap.find x env), t) else (Var x, t)
    | Raise e -> (Error, t)
    | EFun (arg, e) -> 
      let (g, t) = exp_to_rg (kill_env_by_arg arg env) t e in
      begin match g with
      | Fun (id, args, g) -> (Fun (id, update_arg args arg, g), t)
      | _ -> (Fun (new_id (), update_arg BatSet.empty arg, g), t)
      end
    | EList es -> 
      let (gs, t) = list_to_rg env t es in
      (List gs, t)
    | ECtor (x, es) -> 
      let (gs, t) = list_to_rg env t es in
      (Ctor (x, gs), t)
    | ETuple es -> 
      let (gs, t) = list_to_rg env t es in
      (Tuple gs, t)
    | MINUS e ->
      let (g, t) = exp_to_rg env t e in
      (Minus g, t)
    | NOT  e ->
      let (g, t) = exp_to_rg env t e in
      (Not g, t)
    | ADD (e1, e2) ->
      let ((g1, g2), t) = tuple_to_rg env t (e1, e2) in
      (Aop (Add, g1, g2), t)
    | SUB (e1, e2) ->
      let ((g1, g2), t) = tuple_to_rg env t (e1, e2) in
      (Aop (Sub, g1, g2), t)
    | MUL (e1, e2) ->
      let ((g1, g2), t) = tuple_to_rg env t (e1, e2) in
      (Aop (Mul, g1, g2), t)
    | DIV (e1, e2) ->
      let ((g1, g2), t) = tuple_to_rg env t (e1, e2) in
      (Aop (Div, g1, g2), t)
    | MOD (e1, e2) ->
      let ((g1, g2), t) = tuple_to_rg env t (e1, e2) in
      (Aop (Mod, g1, g2), t)
    | OR (e1, e2) ->
      let ((g1, g2), t) = tuple_to_rg env t (e1, e2) in
      (Bop (Or, g1, g2), t)
    | AND (e1, e2) ->
      let ((g1, g2), t) = tuple_to_rg env t (e1, e2) in
      (Bop (And, g1, g2), t)
    | LESS (e1, e2) ->
      let ((g1, g2), t) = tuple_to_rg env t (e1, e2) in
      (ABop (Lt, g1, g2), t)
    | LARGER (e1, e2) ->
      let ((g1, g2), t) = tuple_to_rg env t (e1, e2) in
      (ABop (Gt, g1, g2), t)
    | LESSEQ (e1, e2) ->
      let ((g1, g2), t) = tuple_to_rg env t (e1, e2) in
      (ABop (Le, g1, g2), t)
    | LARGEREQ (e1, e2) ->
      let ((g1, g2), t) = tuple_to_rg env t (e1, e2) in
      (ABop (Ge, g1, g2), t)
    | EQUAL (e1, e2) ->
      let ((g1, g2), t) = tuple_to_rg env t (e1, e2) in
      (EQop (Eq, g1, g2), t)
    | NOTEQ (e1, e2) ->
      let ((g1, g2), t) = tuple_to_rg env t (e1, e2) in
      (EQop (NEq, g1, g2), t)
    | AT (e1, e2) ->
      let ((g1, g2), t) = tuple_to_rg env t (e1, e2) in
      (App (g1, g2), t)
    | DOUBLECOLON (e1, e2) ->
      let ((g1, g2), t) = tuple_to_rg env t (e1, e2) in
      (Cons (g1, g2), t)
    | STRCON (e1, e2) ->
      let ((g1, g2), t) = tuple_to_rg env t (e1, e2) in
      (StrCons (g1, g2), t)
    | EApp (e1, e2) ->
      let ((g1, g2), t) = tuple_to_rg env t (e1, e2) in
      (Apply (g1, g2), t)
    | ELet (f, is_rec, args, typ, e1, e2) ->
     (* Kill environment *)
      let func_env = List.fold_left (fun env arg -> kill_env_by_arg arg env) (if is_rec then kill_env_by_binding f env else env) args in
      (* If f is a recursive function, define its entry point early *)
      let func_id = new_id () in
      let func_env = 
        if is_rec && (args <> [] || is_fun typ) then gen_env_by_binding (f, func_id) func_env else func_env
      in
      (* TODO : binding should be generalized *)
      let (g1, t) = exp_to_rg func_env t e1 in   
      begin match f with
      | BindOne f ->
        if (args <> [] || is_fun typ) then  
          (* f is a function *)    
          begin match g1 with
          | Fun (id, arg_info, g) -> 
            let id = if is_rec then func_id else id in
            exp_to_rg (gen_env (f, id) env) (BatSet.add (Fun (id, update_args arg_info args, g)) t) e2
          | _ -> 
            let id = if is_rec then func_id else new_id () in
            exp_to_rg (gen_env (f, id) env) (BatSet.add (Fun (id, update_args BatSet.empty args, g1)) t) e2
          end
        else 
          let (g2, t) = exp_to_rg (kill_env f env) t e2 in
          (Let (BindOne f, g1, g2), t)
      | _ -> 
        let (g2, t) = exp_to_rg (kill_env_by_binding f env) t e2 in
        (Let (f, g1, g2), t)
      end
  (*
      (* Making graph *)
      let (g1, t) = exp_to_rg func_env t e1 in
      begin match g1 with
      | Fun (id, args, g) -> 
        exp_to_rg (gen_env_by_binding (f, id) env) (BatSet.add (Fun (id, update_args args xs, g)) t) e2 
      | _ -> 
        if xs <> [] then
          exp_to_rg (gen_env_by_binding (f, id) env) (BatSet.add (Fun (id, update_args BatSet.empty xs, g1)) t) e2 
        else
          (* Value binding *)
          let (g2, t) = exp_to_rg (kill_env_by_binding f env) t e2 in
          (Let (f, g1, g2), t)
      end
  *)
    | EBlock (is_rec, ds, e) ->
      let (env, t, ds) = bindings_to_graph env t ds in 
      exp_to_rg env t (bindings_to_exp ds e)
    | EMatch (e, bs) ->
      let (g, t) = exp_to_rg env t e in
      let (bs, t) = List.fold_left (fun (bs, t) (p, e) ->
        let (g', t) = exp_to_rg env t e in
        let ctx = Sig (EQop (Eq, g, pat_to_node p)) in
        ((ctx, g')::bs, t)
      ) ([], t) (flatten_branch bs) in
      (Branch (List.rev bs), t)
    | IF (e1, e2, e3) ->
      let (g1, t) = exp_to_rg env t e1 in
      let (g2, t) = exp_to_rg env t e2 in
      let (g3, t) = exp_to_rg env t e3 in
      let ctx = Sig g1 in
      (Branch [(ctx, g2); (Neg ctx, g3)], t)
  in
  ((l, unode), t)

and tuple_to_rg : rg_env -> t -> lexp * lexp -> (node * node) * t
= fun env t (e1, e2) ->
  let (g1, t) = exp_to_rg env t e1 in
  let (g2, t) = exp_to_rg env t e2 in
  ((g1, g2), t)

and list_to_rg : rg_env -> t -> lexp list -> node list * t
= fun env t es ->
  match es with
  | [] -> ([], t)
  | hd::tl ->
    let (g, t) = exp_to_rg env t hd in
    let (gs, t) = list_to_rg env t tl in 
    (g::gs, t)

and bindings_to_graph : rg_env -> t -> binding list -> rg_env * t * binding list
= fun env t ds ->
  (* Initialize *)
  let env = List.fold_left (fun env (f, is_rec, args, typ, e) ->
    if args <> [] || is_fun typ then gen_env_by_binding (f, new_id ()) env else env
  ) env ds in
  (* Making graph of function *)
  List.fold_left (fun (env, t, ds) (f, is_rec, args, typ, e) -> 
    match f with
    | BindOne f ->
      if BatMap.mem f env then  
        (* f is a function *)       
        let func_env = List.fold_left (fun env arg -> kill_env_by_arg arg env) env args in
        let (g, t) = exp_to_rg func_env t e in
        let id = BatMap.find f env in
        let t = 
          begin match g with
          | Fun (_, arg_info, g) -> (BatSet.add (Fun (id, update_args arg_info args, g)) t)
          | _ -> if args <> [] then BatSet.add (Fun (id, update_args BatSet.empty args, g)) t else t
          end
        in
        (env, t, ds)
      else (env, t, (BindOne f, is_rec, args, typ, e)::ds)
    | _ -> (env, t, (f, is_rec, args, typ, e)::ds)
  ) (env, t, []) ds

(* Kill environment *)
  let func_env = List.fold_left (fun env arg -> kill_env_by_arg arg env) (if is_rec then kill_env_by_binding f env else env) args in
  (* If f is a recursive function, define its entry point early *)
  let func_id = new_id () in
  let func_env = 
    if is_rec && (args <> [] || is_fun typ) then gen_env_by_binding (f, func_id) func_env else func_env
  in
  (* TODO : binding should be generalized *)
  let (g1, t) = exp_to_rg func_env t e1 in   
  begin match f with
  | BindOne f ->
    if (args <> [] || is_fun typ) then  
      (* f is a function *)    
      begin match g1 with
      | Fun (id, arg_info, g) -> 
        let id = if is_rec then func_id else id in
        exp_to_rg (gen_env (f, id) env) (BatSet.add (Fun (id, update_args arg_info args, g)) t) e2
      | _ -> 
        let id = if is_rec then func_id else new_id () in
        exp_to_rg (gen_env (f, id) env) (BatSet.add (Fun (id, update_args BatSet.empty args, g1)) t) e2
      end
    else 
      let (g2, t) = exp_to_rg (kill_env f env) t e2 in
      (Let (BindOne f, g1, g2), t)
  | _ -> 
    let (g2, t) = exp_to_rg (kill_env_by_binding f env) t e2 in
    (Let (f, g1, g2), t)
  end

let rec decl_to_rg : rg_env -> t -> decl -> rg_env * t
= fun env t decl ->
  match decl with
  | DLet (f, is_rec, args, typ, e) -> 
    (* Kill environment *)
    let func_env = List.fold_left (fun env arg -> kill_env_by_arg arg env) (if is_rec then kill_env_by_binding f env else env) args in
    (* If f is a recursive function, define its entry point early *)
    let func_id = new_id () in
    let func_env = 
      if is_rec && (args <> [] || is_fun typ) then gen_env_by_binding (f, func_id) func_env else func_env
    in
    (* TODO : binding should be generalized *)
    let (g1, t) = exp_to_rg func_env t e in   
    begin match f with
    | BindOne f ->
      if (args <> [] || is_fun typ) then  
        (* f is a function *)    
        begin match g1 with
        | Fun (id, arg_info, g) -> 
          let id = if is_rec then func_id else id in
          (gen_env (f, id) env, BatSet.add (Fun (id, update_args arg_info args, g)) t)
        | _ -> 
          let id = if is_rec then func_id else new_id () in
          (gen_env (f, id) env), (BatSet.add (Fun (id, update_args BatSet.empty args, g1)) t)
        end
      else (env, t)
    | _ -> (env, t)
    end
  (*
    let func_env = List.fold_left (fun env arg -> kill_env_by_arg arg env) (if is_rec then kill_env_by_binding f env else env) args in
    let (g, t) = exp_to_rg func_env t e in
    begin match g with
    | Fun (id, arg_info, g) -> (gen_env_by_binding (f, id) env, BatSet.add (Fun (id, update_args arg_info args, g)) t)
    | _ -> 
      if args <> [] then
        let id = new_id () in
        (gen_env_by_binding (f, id) env, BatSet.add (Fun (id, update_args BatSet.empty args, g)) t)
      else
        (env, t)
    end
  *)
  | DBlock (is_rec, ds) -> 
    let (env, t, _) = bindings_to_graph env t ds in 
    (env, t)
  | _ -> (env, t)

(* Extract (rg, entry point) of a given program *)
let run : prog -> t * graph_id
= fun pgm -> 
  let (env, t) = List.fold_left (fun (env, t) decl -> decl_to_rg env t decl) (BatMap.empty, BatSet.empty) (T.run pgm) in
  let _ = 
    print_endline ("Env : ");
    print_endline (Print.string_of_map (fun x -> x) (fun id -> string_of_int id) env);
    print t
  in
  (t, BatMap.find !Options.opt_entry_func env)