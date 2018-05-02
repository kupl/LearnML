open Lang
open Util
open Symbol_lang

let empty_env = BatMap.empty
let extend_env (x,t) env = BatMap.add x t env
let find_env x env = BatMap.find x env

let infinite_count = ref 0
let start_time = ref 0.0

(********************* Partial Eval Helper ***********************)
(* Block task *)
let rec is_fun : symbolic_value -> bool
= fun sv ->
  match sv with
  | List svs | Tuple svs | Ctor (_, svs) -> List.exists is_fun svs
  | Fun _ | FunRec _ | FunBlock _ -> true
  | _ -> false

let rec update_closure : symbolic_env -> symbolic_value -> symbolic_value
= fun env sv ->
  match sv with
  | List svs -> List (List.map (update_closure env) svs)
  | Tuple svs -> Tuple (List.map (update_closure env) svs)
  | Ctor (x, svs) -> Ctor (x, List.map (update_closure env) svs)
  | Fun (x, e, closure) -> Fun (x, e, env)
  | FunRec (f, x, e, closure) -> FunRec (f, x, e, env)
  | FunBlock (f, svs) ->  
    let (xs, svs) = List.split svs in
    let svs = List.map (update_closure env) svs in
    FunBlock (f, List.combine xs svs)
  | _ -> sv

let rec find_callee : id -> (id * symbolic_value) list -> symbolic_value
= fun x svs ->
  match svs with
  | [] -> raise (Failure "not found")
  | (y, sv)::tl -> if x = y then sv else find_callee x tl

let bind_block : symbolic_env -> (id * symbolic_value) list -> symbolic_env
= fun env svs ->
  let (xs, _) = List.split svs in
  List.fold_left (fun env x -> extend_env (x, FunBlock (x, svs)) env) env xs

(* Argument binding *)

let rec arg_binding : symbolic_env -> arg -> symbolic_value -> symbolic_env
= fun env arg sv ->
  match (arg, sv) with
  | ArgUnder t, _ -> env
  | ArgOne (x, t), _ -> extend_env (x, sv) env
  | ArgTuple xs, Symbol _ ->
    let svs = List.map (fun x -> fresh_symbol ()) xs in
    arg_binding env arg (Tuple svs)
  | ArgTuple xs, Tuple svs -> 
    (
      try List.fold_left2 arg_binding env xs svs 
      with 
      | Invalid_argument _ -> raise (Failure "argument binding failure - tuples are not compatible")
      | _ -> raise (Failure "Stack overflow during evaluation (looping recursion?)")
    )
  | _ -> raise (Failure "argument binding failure")

let rec let_binding : symbolic_env -> let_bind -> symbolic_value -> symbolic_env
= fun env x sv ->
  match (x, sv) with
  | BindUnder, _ -> env
  | BindOne x, _ -> extend_env (x, sv) env
  | BindTuple xs, Tuple svs -> 
    (
      try List.fold_left2 let_binding env xs svs 
      with 
      | Invalid_argument _ -> raise (Failure "argument binding failure - tuples are not compatible")
      | _ -> raise (Failure "Stack overflow during evaluation (looping recursion?)")
    )
  | _ -> raise (Failure "let binding failure")

(* Pattern Matching *)
let rec find_first_branch : symbolic_value -> branch list -> (pat * lexp)
= fun sv bs -> 
  match bs with 
  | [] -> raise (Failure "Pattern matching failure")
  | (p, e)::tl -> if (pattern_match sv p) then (p, e) else find_first_branch sv tl

and pattern_match : symbolic_value -> pat -> bool
= fun sv p ->
  match (sv, p) with
  | Int n1, PInt n2 -> n1 = n2
  | Bool b1, PBool b2 -> b1 = b2
  | List l1, PList l2 -> pattern_match_list l1 l2 
  | Tuple l1, PTuple l2 -> pattern_match_list l1 l2
  | Ctor (x1, l1), PCtor (x2, l2) -> (x1 = x2) && pattern_match_list l1 l2
  | List [], PCons (phd::ptl) -> if ptl = [] then (pattern_match sv phd) else false
  | List (vhd::vtl), PCons (phd::ptl) -> if ptl = [] then (pattern_match sv phd) else (pattern_match vhd phd) && (pattern_match (List vtl) (PCons ptl))
  | _, PVar x -> true
  | _, PUnder -> true
  | _, Pats pl -> (try List.exists (pattern_match sv) pl with _ -> false)
  | _ -> false

and pattern_match_list : symbolic_value list -> pat list -> bool
= fun svs ps -> try List.for_all2 pattern_match svs ps with _ -> false

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

let rec bind_pat : symbolic_env -> symbolic_value -> pat -> symbolic_env
= fun env sv p ->
  match (sv, p) with
  | _, PInt n2 -> env
  | _, PBool b2 -> env
  | _, PUnder -> env
  | _, PVar x -> extend_env (x, sv) env
  | _, Pats ps -> if check_patterns ps then bind_pat env sv (List.find (pattern_match sv) ps) else raise (Failure "Invalid pattern list")
  | List l1, PList l2 -> bind_pat_list env l1 l2 
  | Tuple l1, PTuple l2 -> bind_pat_list env l1 l2
  | Ctor (x1, l1), PCtor (x2, l2) -> bind_pat_list env l1 l2
  | List [], PCons (phd::ptl) -> if ptl = [] then (bind_pat env sv phd) else raise (Failure "Pattern binding failure")
  | List (vhd::vtl), PCons (phd::ptl) -> if ptl = [] then (bind_pat env sv phd) else bind_pat (bind_pat env vhd phd) (List vtl) (PCons ptl)
  | _ -> raise (Failure "Pattern binding failure")

and bind_pat_list : symbolic_env -> symbolic_value list -> pat list -> symbolic_env
= fun env svs ps -> List.fold_left2 bind_pat env svs ps

(********************* Partial Eval ***********************)
(* Utility functions *)
let rec is_symbol_const : symbolic_value -> bool
= fun sv ->
  match sv with
  | Unit | Int _ | Bool _ -> true | Str _ | Fun _ | FunRec _ | FunBlock _ -> true
  | List svs | Tuple svs | Ctor (_, svs) -> List.for_all is_symbol_const svs
  | Exn sv -> is_symbol_const sv
  | _ -> false

(* Symbolic value normalize *)
let rec normalize : symbolic_value -> symbolic_value
= fun sv ->
  match sv with
  (* Inductive data *)
  | List svs -> List (List.map normalize svs)
  | Tuple svs -> Tuple (List.map normalize svs)
  | Ctor (x, svs) -> Ctor (x, List.map normalize svs)
  | Exn sv -> Exn (normalize sv)
  (* unary operation *)
  | Minus sv ->
    let sv = normalize sv in
    begin match sv with
    | Int n -> Int (-n)
    | _ -> Minus sv
    end
  | Not sv ->
    let sv = normalize sv in
    begin match sv with
    | Bool b -> Bool (not b)
    | _ -> Not sv
    end
  (* binary operation *)
  | Aop (op, sv1, sv2) -> normalize_aop op (normalize sv1) (normalize sv2)
  | Bop (op, sv1, sv2) -> normalize_bop op (normalize sv1) (normalize sv2)
  | ABop (op, sv1, sv2) -> normalize_abop op (normalize sv1) (normalize sv2)
  | EQop (op, sv1, sv2) -> normalize_eqop op (normalize sv1) (normalize sv2)
  | Cons (sv1, sv2) ->
    let (sv1, sv2) = (normalize sv1, normalize sv2) in
    begin match (sv1, sv2) with
    | _ , List svs -> List (sv1::svs)
    | _ -> Cons (sv1, sv2)
    end
  | Append (sv1, sv2) ->
    let (sv1, sv2) = (normalize sv1, normalize sv2) in
    begin match (sv1, sv2) with
    | List svs1, List svs2 -> List (svs1@svs2)
    | _ -> Append (sv1, sv2)
    end
  | Strcon (sv1, sv2) ->
    let (sv1, sv2) = (normalize sv1, normalize sv2) in
    begin match (sv1, sv2) with
    | Str s1, Str s2 -> Str (s1 ^ s2)
    | _ -> Strcon (sv1, sv2)
    end
  (* Const *)
  | _ -> sv

and normalize_aop : operator -> symbolic_value -> symbolic_value -> symbolic_value
= fun op sv1 sv2 ->
  match op with
  | Add ->
    begin match (sv1, sv2) with
    | Int n1, Int n2 -> Int (n1 + n2)
    | Int 0, sv | sv, Int 0 -> sv
    | Int n1, Aop (Add, Int n2, sv') | Int n1, Aop (Add, sv', Int n2) -> Aop (Add, Int (n1 + n2), sv')
    | Aop (Add, Int n1, sv'), Int n2 | Aop (Add, sv', Int n1), Int n2 -> Aop (Add, Int (n1 + n2), sv')
    | Symbol _, _ | _, Symbol _ -> fresh_symbol ()
    | _ -> Aop (op, sv1, sv2)
    end
  | Sub ->
    begin match (sv1, sv2) with
    | Int n1, Int n2 -> Int (n1 - n2)
    | Int 0, _ -> Minus sv2
    | _, Int 0 -> sv1
    | Int n1, Aop (Sub, Int n2, sv') | Int n1, Aop (Sub, sv', Int n2) -> Aop (Sub, Int (n1 - n2), sv')
    | Aop (Sub, Int n1, sv'), Int n2 | Aop (Sub, sv', Int n1), Int n2 -> Aop (Sub, Int (n1 - n2), sv')
    | Symbol _, _ | _, Symbol _ -> fresh_symbol ()
    | _ -> Aop (op, sv1, sv2)
    end
  | Mul ->
    begin match (sv1, sv2) with
    | Int n1, Int n2 -> Int (n1 * n2)
    | Int 0, _ | _, Int 0 -> Int 0
    | Int 1, sv | sv, Int 1 -> sv
    | Int n1, Aop (Mul, Int n2, sv') | Int n1, Aop (Mul, sv', Int n2) -> Aop (Mul, Int (n1 * n2), sv')
    | Aop (Mul, Int n1, sv'), Int n2 | Aop (Mul, sv', Int n1), Int n2  -> Aop (Mul, Int (n1 * n2), sv')
    | Symbol _, Symbol _ -> fresh_symbol ()
    | _ -> Aop (op, sv1, sv2)
    end
  | Div ->
    begin match (sv1, sv2) with
    | _, Int 0 -> raise (Failure "Division_by_zero.")
    | Int n1, Int n2 -> Int (n1 / n2)
    | Int 0, _ -> Int 0
    | _, Int 1 -> sv1
    | Aop (Div, sv', Int n1), Int n2  -> Aop (Div, sv' , Int (n1 * n2))
    | Symbol _, _ -> fresh_symbol ()
    | _ -> Aop (op, sv1, sv2)
    end
  | Mod ->
    begin match (sv1, sv2) with
    | _, Int 0 -> raise (Failure "Division_by_zero.")
    | Int n1, Int n2 -> Int (n1 mod n2)
    | Int 0, _ -> Int 0
    | _, Int 1 -> Int 0
    | Symbol _, Symbol _ -> fresh_symbol ()
    | _ -> Aop (op, sv1, sv2)
    end

and normalize_bop : combinator -> symbolic_value -> symbolic_value -> symbolic_value
= fun op sv1 sv2 ->
  match op with
  | And ->
    begin match (sv1, sv2) with
    | Bool b1, Bool b2 -> Bool (b1 && b2)
    | Bool true, sv | sv, Bool true -> sv
    | Bool false, _ | _, Bool false -> Bool false
    | _ -> Bop (op, sv1, sv2)
    end
  | Or ->
    begin match (sv1, sv2) with
    | Bool b1, Bool b2 -> Bool (b1 || b2)
    | Bool false, sv | sv, Bool false -> sv
    | Bool true, _ | _, Bool true -> Bool true
    | _ -> Bop (op, sv1, sv2)
    end

and normalize_abop : comparator -> symbolic_value -> symbolic_value -> symbolic_value
= fun op sv1 sv2 ->
  match (sv1, sv2) with
  | Int n1, Int n2 ->
    begin match op with
    | Lt -> Bool (n1 < n2)
    | Gt -> Bool (n1 > n2)
    | Le -> Bool (n1 <= n2)
    | Ge -> Bool (n1 >= n2)
    end
  | _ -> ABop (op, sv1, sv2)

(* Validity ??? *)
and normalize_eqop : eq_operator -> symbolic_value -> symbolic_value -> symbolic_value
= fun op sv1 sv2 ->
  (* two inputs are already normalized *)
  if (is_fun sv1) || (is_fun sv2) then 
    (* Function check *)
    raise (Failure "Unable to check functions equality")
  else if (is_symbol_const sv1) && (is_symbol_const sv2) then
    (* If the two values do not have hole then compare them directly *)
    begin match op with
    | Eq -> Bool (sv1 = sv2)
    | NEq -> Bool (sv1 <> sv2)
    end
  else
    begin match (sv1, sv2) with
    | Symbol _, _ | _, Symbol _ -> fresh_symbol ()
    | Ctor (x, svs1), Ctor (y, svs2) ->
      if op = Eq then 
        if (x <> y) then Bool false else normalize_eqop Eq ((List.hd svs1)) ((List.hd svs2))
      else
        if (x = y) then Bool false else normalize_eqop NEq ((List.hd svs1)) ((List.hd svs2))
    | Ctor (x, svs), _ | _, Ctor (x, svs) -> 
      if op = Eq then Bool false else Bool true
    | _ -> EQop (op, sv1, sv2) 
    end 

(* Encoding *)
let rec partial_eval_exp : symbolic_env -> lexp -> symbolic_value
= fun env (_, exp) ->
  (*print_endline (Print.exp_to_string exp);*)
  if (Unix.gettimeofday() -. !start_time >0.20) then 
    let _ = (infinite_count:=!(infinite_count)+1) in
    raise TimeoutError
  else
  let sv = 
    match exp with 
    (* Const *)
    | EUnit -> Unit
    | Const n -> Int n
    | TRUE -> Bool true
    | FALSE -> Bool false
    | String str -> Str str
    | EVar x -> find_env x env
    | EList es -> List (List.map (partial_eval_exp env) es)
    | ETuple es -> Tuple (List.map (partial_eval_exp env) es)
    | ECtor (x, es) -> Ctor (x, List.map (partial_eval_exp env) es)
    | Raise e -> Exn (partial_eval_exp env e)
    | Hole n -> fresh_symbol ()
    | EFun (arg, e) -> Fun (arg, e, env)
    (* Unary Operator *)
    | MINUS e -> Minus (partial_eval_exp env e)
    | NOT e -> Not (partial_eval_exp env e)
    (* Binary Operator *)
    | ADD (e1, e2) -> Aop (Add, partial_eval_exp env e1, partial_eval_exp env e2)
    | SUB (e1, e2) -> Aop (Sub, partial_eval_exp env e1, partial_eval_exp env e2)
    | MUL (e1, e2) -> Aop (Mul, partial_eval_exp env e1, partial_eval_exp env e2)
    | DIV (e1, e2) -> Aop (Div, partial_eval_exp env e1, partial_eval_exp env e2)
    | MOD (e1, e2) -> Aop (Mod, partial_eval_exp env e1, partial_eval_exp env e2)
    | OR (e1, e2) -> Bop (Or, partial_eval_exp env e1, partial_eval_exp env e2)
    | AND (e1, e2) -> Bop (And, partial_eval_exp env e1, partial_eval_exp env e2)
    | LESS (e1, e2) -> ABop (Lt, partial_eval_exp env e1, partial_eval_exp env e2)
    | LESSEQ (e1, e2) -> ABop (Le, partial_eval_exp env e1, partial_eval_exp env e2)
    | LARGER (e1, e2) -> ABop (Gt, partial_eval_exp env e1, partial_eval_exp env e2)
    | LARGEREQ (e1, e2) -> ABop (Ge, partial_eval_exp env e1, partial_eval_exp env e2)
    | EQUAL (e1, e2) -> EQop (Eq, partial_eval_exp env e1, partial_eval_exp env e2)
    | NOTEQ (e1, e2) -> EQop (NEq, partial_eval_exp env e1, partial_eval_exp env e2)
    | DOUBLECOLON (e1, e2) -> Cons (partial_eval_exp env e1, partial_eval_exp env e2)
    | AT (e1, e2) -> Append (partial_eval_exp env e1, partial_eval_exp env e2)
    | STRCON (e1,e2) -> Strcon (partial_eval_exp env e1, partial_eval_exp env e2)
    (* Branch expressions => normalize & condition hole *)
    | IF (e1, e2, e3) ->
      let sv1 = partial_eval_exp env e1 in
      if is_symbol_const sv1 then
        begin match sv1 with
        | Bool b -> if b then partial_eval_exp env e2 else partial_eval_exp env e3
        | _ -> raise (Failure "if_type error")
        end
      else fresh_symbol () (* Check SAT?? *)
    | EMatch (e, bs) ->
      let sv = partial_eval_exp env e in
      if is_symbol_const sv then 
        let (p, ex) = find_first_branch sv bs in
        partial_eval_exp (bind_pat env sv p) ex
      else fresh_symbol () (* Merge all branchs *)
    (* Binding expressions *)
    | ELet (f, is_rec, args, typ, e1, e2) -> 
      begin match args with
      |[] ->
        (* variable binding *)
        if is_rec then
          let sv1 = partial_eval_exp env e1 in
          begin match f with
          | BindOne f ->
            begin match sv1 with
            | Fun (x, e, closure) -> partial_eval_exp (extend_env (f, FunRec (f, x, e, closure)) env) e2
            | _ -> partial_eval_exp (extend_env (f, sv1) env) e2
            end
          | _ -> raise (Failure "Only variables are allowed as left-hand side of `let rec'")
          end
        else partial_eval_exp (let_binding env f (partial_eval_exp env e1)) e2
      |_ ->
        (* function binding *)
        let rec binding : arg list -> lexp -> lexp 
        = fun xs e -> 
          begin match xs with
          | [] -> e
          | hd::tl -> (gen_label(),EFun (hd, binding tl e))
          end 
        in
        let x = List.hd args in
        let vf =
          if is_rec then
            begin match f with
            | BindOne f -> FunRec (f, x, (binding (List.tl args) e1), env)
            | _ -> raise (Failure "Only variables are allowed as left-hand side of `let rec'")
            end
          else
            Fun (x, (binding (List.tl args) e1), env)
        in
        partial_eval_exp (let_binding env f vf) e2
      end
    | EBlock (is_rec, bindings, e2) -> 
      let env = 
        begin match is_rec with
        | true ->
          let (func_map, const_map) = List.fold_left (
            fun (func_map, const_map) (f, is_rec, args, typ, exp) ->
              match f with 
              | BindOne x ->
                let sv = partial_eval_exp env (gen_label(),ELet (BindOne x, is_rec, args, typ, exp, (gen_label(), EVar x))) in
                if is_fun sv then ((x, sv)::func_map, const_map) else (func_map, (x, sv)::const_map)
              | _ -> raise (Failure "Only variables are allowed as left-hand side of `let rec'")
          ) ([], []) bindings
          in
          (* constant mapping *)
          let init_env = List.fold_left (fun env (x, c) -> extend_env (x, c) env) env const_map in
          (* update each function's closure *)
          let func_map = List.map (fun (x, sv) -> (x, update_closure init_env sv)) func_map in
          (* block mapping *)
          List.fold_left (fun env (x, sv) -> extend_env (x, FunBlock (x, func_map)) env) init_env func_map
        | false ->
          let svs = List.map (fun (f, is_rec, args, typ, e) -> partial_eval_exp env (gen_label(), ELet (f, is_rec, args, typ, e, let_to_exp f))) bindings in
          List.fold_left2 (fun env (f, is_rec, args, typ, e) sv -> let_binding env f sv) env bindings svs
        end
      in partial_eval_exp env e2
    (* Function call *)
    | EApp (e1, e2) ->
      let (sv1, sv2) = (partial_eval_exp env e1, partial_eval_exp env e2) in
      begin match sv1 with
      | Fun (x, e, closure) -> 
        (*print_endline ("Arg : " ^ Print.symbol_to_string sv2);*)
        partial_eval_exp (arg_binding closure x sv2) e
      | FunRec (f, x, e, closure) -> 
        (*print_endline ("Arg : " ^ Print.symbol_to_string sv2);*)
        partial_eval_exp (extend_env (f, sv1) (arg_binding closure x sv2)) e
      | FunBlock (f, svs) ->
        let sv = find_callee f svs in
        begin match sv with
        | FunRec (f, x, e, closure) -> 
          let block_env = bind_block closure svs in
          partial_eval_exp (arg_binding block_env x sv2) e
        | _ -> raise (Failure "mutually recursive function call error")
        end
      | Symbol _ -> fresh_symbol () (* ??? *)
      | _ -> raise (Failure "function_call error")
      end
    in 
    normalize sv

let rec partial_eval_decl : symbolic_env -> decl -> symbolic_env
= fun env decl ->
  match decl with
  | DLet (f, is_rec, args, typ, e) ->
    let exp = 
      begin match f with 
      | BindUnder -> e
      | _ -> (gen_label(), ELet (f, is_rec, args, typ, e, let_to_exp f))
      end
    in
    let_binding env f (partial_eval_exp env exp)
  | DBlock (is_rec, bindings) ->
    if is_rec then
      let (func_map, const_map) = List.fold_left (
        fun (func_map, const_map) (f, is_rec, args, typ, exp) ->
        begin match f with 
        | BindOne x ->
          let sv = partial_eval_exp env (gen_label(), ELet (BindOne x, is_rec, args, typ, exp, (gen_label(), EVar x))) in
          if is_fun sv then ((x, sv)::func_map, const_map) else (func_map, (x, sv)::const_map)
        | _ -> raise (Failure "Only variables are allowed as left-hand side of `let rec'")
        end
      ) ([], []) bindings
      in
      (* constant mapping *)
      let init_env = List.fold_left (fun env (x, c) -> extend_env (x, c) env) env const_map in
      (* update each function's closure *)
      let func_map = List.map (fun (x, sv) -> (x, update_closure init_env sv)) func_map in
      (* block mapping *)
      List.fold_left (fun env (x, sv) -> extend_env (x, FunBlock (x, func_map)) env) init_env func_map
    else
      let svs = List.map (fun (f, is_rec, args, typ, e) -> partial_eval_exp env (gen_label(), ELet (f, is_rec, args, typ, e, let_to_exp f))) bindings in
      List.fold_left2 (fun env (f, is_rec, args, typ, e) sv -> let_binding env f sv) env bindings svs
  | _ -> env

let partial_run : prog -> symbolic_env
= fun decls -> 
  start_time:=Unix.gettimeofday();
  let init_env = List.fold_left partial_eval_decl empty_env (External.init_prog) in
  start_time:=Unix.gettimeofday();
  let env = List.fold_left partial_eval_decl init_env decls in
  BatMap.diff env init_env

(********************* Constraint Generation ***********************)
(* Value to Symbol *)
let rec value_to_symbol : value -> symbolic_value
= fun value ->
  match value with
  | VUnit -> Unit
  | VInt n -> Int n
  | VBool b -> Bool b
  | VList vs -> List (List.map value_to_symbol vs)
  | VTuple vs -> Tuple (List.map value_to_symbol vs)
  | VCtor (x, vs) -> Ctor (x, (List.map value_to_symbol vs))
  | VString str -> Str str
  | VHole n -> Symbol n
  | _ -> raise (Failure "Output must be a constant")

(* Execution result to Symbolic value*)
let rec symbolic_execution : prog -> lexp list -> symbolic_value
= fun pgm input ->
  let res_var = "__res__" in
  let pgm = pgm@(External.grading_prog) in
  let pgm' = pgm @ [(DLet (BindOne res_var,false,[],fresh_tvar(),(appify (gen_label(), (EVar !Options.opt_entry_func)) input)))] in
  let env = partial_run pgm' in
  find_env res_var env

let gen_constraint : prog -> example -> symbolic_value
= fun pgm (input, output) ->
  try
    let sv1 = symbolic_execution pgm input in
    let sv2 = value_to_symbol output in
    normalize (EQop (Eq, sv1, sv2))
  with _ -> Bool false
