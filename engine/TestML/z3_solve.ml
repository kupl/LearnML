open Util
open Lang
open Symbol_lang2
open Z3
open Z3.Solver

(* type constructor table (for constraint generation) *)
module CtorTable = struct
  (* 
    Type name => Constructors name => Constructor arugments 
    ex) exp -> [ADD => exp * exp | SUB => exp * exp | Int => int | X => .]
  *)
  type t = (id, (id, typ list) BatMap.t) BatMap.t

  let generation : t -> prog -> t
  = fun t decls ->
    let rec generation2 : t -> decl -> t
    = fun t decl ->
      match decl with
      | DData (x, ctors) -> 
        let ctors = List.fold_left (fun ctors (x, typs) -> 
          BatMap.add x typs ctors
        ) BatMap.empty ctors
        in
        BatMap.add x ctors t
      | TBlock decls -> List.fold_left generation2 t decls
      | _ -> t
    in
    let decls = Type.Converter.convert Type.Converter.empty decls in
    List.fold_left generation2 t decls
    
  let print : t -> unit
  = fun t ->
    BatMap.iter (fun x ctors -> 
      print_endline (x ^ " -> ");
      BatMap.iter (fun x typs ->
        print_endline (x ^ "=>");
        List.iter (fun ty -> print_endline (Print.type_to_string ty)) typs
      ) ctors
    ) t
end

(* Translate given symbolic constraint into a Z3 equation *)  
module Z3_Translator = struct
    
  (* helper function *)
  let sort_to_symbol sort = Z3.Sort.get_name sort
  let counter = ref 0
  let new_counter () = (counter := !counter + 1); !counter
  (* context => if the formula is too hard to solve in 0.05 sec, fail to find a counter example *)
  let new_ctx () = mk_context [("timeout", "50")]

  (* sort *)
  let int_sort ctx = Z3.Arithmetic.Integer.mk_sort ctx
  let bool_sort ctx = Z3.Boolean.mk_sort ctx
  let string_sort ctx = Z3.Seq.mk_string_sort ctx
  let list_sort ctx symbol sort = Z3.Z3List.mk_sort ctx (Z3.Symbol.mk_string ctx ((Z3.Symbol.to_string symbol) ^ "_List")) sort
  let tuple_sort ctx symbol sorts = Z3.Tuple.mk_sort ctx symbol (List.map sort_to_symbol sorts) sorts
  let get_expr_sort expr = Z3.Expr.get_sort expr

  (* Symbolic variable (identifier) *)
  let mk_symbol ctx id sort = Z3.Expr.mk_const_s ctx id sort

  (* Constant *)
  let int_const ctx n = Z3.Arithmetic.Integer.mk_numeral_i ctx n
  let bool_const ctx b = Z3.Boolean.mk_val ctx b
  let str_const ctx str = Z3.Seq.mk_string ctx str

  (* aop *)
  let mk_minus ctx expr = Z3.Arithmetic.mk_unary_minus ctx expr
  let mk_add ctx expr1 expr2 = Z3.Arithmetic.mk_add ctx [expr1; expr2]
  let mk_sub ctx expr1 expr2 = Z3.Arithmetic.mk_sub ctx [expr1; expr2]
  let mk_mul ctx expr1 expr2 = Z3.Arithmetic.mk_mul ctx [expr1; expr2]
  let mk_div ctx expr1 expr2 = Z3.Arithmetic.mk_div ctx expr1 expr2
  let mk_mod ctx expr1 expr2 = Z3.Arithmetic.Integer.mk_mod ctx expr1 expr2

  (* bop *)
  let mk_not ctx expr = Z3.Boolean.mk_not ctx expr
  let mk_and ctx expr1 expr2 = Z3.Boolean.mk_and ctx [expr1; expr2]
  let mk_or ctx expr1 expr2 = Z3.Boolean.mk_or ctx [expr1; expr2]
  let mk_lt ctx expr1 expr2 = Z3.Arithmetic.mk_lt ctx expr1 expr2
  let mk_le ctx expr1 expr2 = Z3.Arithmetic.mk_le ctx expr1 expr2
  let mk_gt ctx expr1 expr2 = Z3.Arithmetic.mk_gt ctx expr1 expr2
  let mk_ge ctx expr1 expr2 = Z3.Arithmetic.mk_ge ctx expr1 expr2
  let mk_eq ctx expr1 expr2 = Z3.Boolean.mk_eq ctx expr1 expr2
  let mk_neq ctx expr1 expr2 = (mk_not ctx (mk_eq ctx expr1 expr2))
  let mk_imply ctx expr1 expr2 = Z3.Boolean.mk_implies ctx expr1 expr2

  (* list  *)
  let mk_nil sort = Z3.Z3List.nil sort
  let mk_cons ctx sort expr1 expr2 =
    let cons_func = Z3.Z3List.get_cons_decl sort in
    Z3.Expr.mk_app ctx cons_func [expr1; expr2]
  
  (* tuple *)
  let mk_tuple ctx sort exprs =
    let tuple_func = Z3.Tuple.get_mk_decl sort in
    Z3.Expr.mk_app ctx tuple_func exprs
  
  (* string *)
  let mk_concat ctx expr1 expr2 = Z3.Seq.mk_seq_concat ctx [expr1; expr2]

  (* constructor *)
  let ctor_app ctx ctor_func exprs = 
    Z3.Expr.mk_app ctx ctor_func exprs
  
  (* Generate a symbolic variable whose type is sort in current context *)
  let symbol_num = ref 0
  let fresh_id () = (symbol_num := !symbol_num + 1); ("#" ^ (string_of_int !symbol_num))
  let int_symbol ctx = mk_symbol ctx (fresh_id ()) (int_sort ctx)
  let bool_symbol ctx = mk_symbol ctx (fresh_id ()) (bool_sort ctx)
  let string_symbol ctx = mk_symbol ctx (fresh_id ()) (string_sort ctx)
  let list_symbol ctx sort = mk_symbol ctx (fresh_id ()) (list_sort ctx (sort_to_symbol sort) sort)
  let tuple_symbol ctx sort = mk_symbol ctx (fresh_id ()) sort
  let ctor_symbol ctx sort = mk_symbol ctx (fresh_id ()) sort

  (* 
    Mapping for User_defined_type_name -> Z3.Sort 
    ex)
      ADD => exp
      SUB => exp
      ...
  *)
  type ctor_map = (id, Z3.Sort.sort) BatMap.t

  (* PP *)
  let print_formula : Z3.Expr.expr list -> unit
  = fun exprs ->
    List.iter (fun expr -> print_endline (Z3.Expr.to_string expr)) exprs

  let print_ctor_map : ctor_map -> unit
  = fun map -> BatMap.iter (fun id sort -> 
    let decls = Z3.Datatype.get_constructors sort in
    print_endline ("Decls : ");
    List.iter (fun decl -> print_endline (Z3.FuncDecl.to_string decl)) decls;
    print_endline (id ^ " -> " ^ Z3.Sort.to_string sort)
  ) map

  (* Init constructor info *)
  let rec typ_to_sort : ctor_map -> Z3.context -> typ -> Z3.Sort.sort
  = fun map ctx typ ->
    match typ with
    | TInt -> int_sort ctx
    | TBool -> bool_sort ctx
    | TString -> string_sort ctx
    | TBase x -> BatMap.find x map
    | TList t -> 
      let sort = typ_to_sort map ctx t in
      list_sort ctx (sort_to_symbol sort) sort 
    | TTuple ts -> 
      let sorts = List.map (typ_to_sort map ctx) ts in
      let tuple_name = Z3.Symbol.mk_string ctx (List.fold_left (fun acc sort -> acc ^ "," ^ Z3.Sort.to_string sort) "" sorts) in
      tuple_sort ctx tuple_name sorts
    | _ -> raise (Failure "Invalid sort") 

  let rec has_ctor_name : typ -> string -> bool
  = fun typ name ->
    match typ with
    | TBase x -> x = name
    | TList t -> has_ctor_name t name
    | TTuple ts -> List.exists (fun t -> has_ctor_name t name) ts
    | TCtor (tbase, ts) -> (has_ctor_name tbase name) || (List.exists (fun t -> has_ctor_name t name) ts)
    | TArr (t1, t2) -> has_ctor_name t1 name || has_ctor_name t2 name
    | _ -> false

  let rec init_ctor_map : Z3.context -> CtorTable.t -> ctor_map -> ctor_map
  = fun ctx table map ->
    BatMap.foldi (fun name ctors map -> 
      let z3_ctors = BatMap.foldi (fun x ts z3_ctors ->
        let z3_ctor = 
          if ts = [] then
            Z3.Datatype.mk_constructor_s ctx x (Z3.Symbol.mk_string ctx x) [] [] []
          else
            let ts = 
              begin match List.hd ts with
              | TTuple ts -> ts
              | x -> [x]
              end
            in 
            let symbols = List.map (fun typ -> (Z3.Symbol.mk_string ctx ((Print.type_to_string typ) ^ string_of_int (new_counter ())))) ts in 
            let sorts = List.map (fun typ -> if has_ctor_name typ name then None else Some (typ_to_sort map ctx typ)) ts in
            let int_list = List.map (fun typ -> if has_ctor_name typ name then 0 else 1) ts in
            
            Z3.Datatype.mk_constructor_s ctx x (Z3.Symbol.mk_string ctx x) symbols sorts int_list
        in
        z3_ctor::z3_ctors
      ) ctors []
      in
      let datatype = Z3.Datatype.mk_sort ctx (Z3.Symbol.mk_string ctx name) z3_ctors in
      let map = BatMap.foldi (fun x _ map ->
        BatMap.add x datatype map
      ) ctors map
      in
      BatMap.add name datatype map
    ) table map

  let rec apply_recursive_datatype : Z3.context -> CtorTable.t -> ctor_map -> ctor_map
  = fun ctx table map ->
    BatMap.foldi (fun name ctors map -> 
      let z3_ctors = BatMap.foldi (fun x ts z3_ctors ->
        let z3_ctor = 
          if ts = [] then
            Z3.Datatype.mk_constructor_s ctx x (Z3.Symbol.mk_string ctx x) [] [] []
          else
            let ts = 
              begin match List.hd ts with
              | TTuple ts -> ts
              | x -> [x]
              end
            in 
            let symbols = List.map (fun typ -> (Z3.Symbol.mk_string ctx ((Print.type_to_string typ) ^ string_of_int (new_counter ())))) ts in 
            let sorts = List.map (fun typ ->
                match typ with
                | TBase x -> if (x = name) then (None) else (Some (typ_to_sort map ctx typ))
                | _ -> (Some (typ_to_sort map ctx typ))
              )  ts
              in
            let int_list = List.map (fun typ -> if typ = TBase name then 0 else 1) ts in
            Z3.Datatype.mk_constructor_s ctx x (Z3.Symbol.mk_string ctx x) symbols sorts int_list
        in
        z3_ctor::z3_ctors
      ) ctors []
      in
      let datatype = Z3.Datatype.mk_sort ctx (Z3.Symbol.mk_string ctx name) z3_ctors in
      let map = BatMap.foldi (fun x _ map ->
        BatMap.add x datatype map
      ) ctors map
      in
      BatMap.add name datatype map
    ) table map
 
  (* Translate given symbolic value into an equation *)
  let rec translate : ctor_map -> Z3.context -> symbolic_value -> (Z3.Expr.expr * Z3.Expr.expr list)
  = fun ctor_map ctx sv ->
    match sv with
    (* Symbol *)
    | SSymbol n ->
      let id = string_symbol ctx in 
      let expr = mk_symbol ctx ("S" ^ string_of_int n) (string_sort ctx) in
      (id, [mk_eq ctx id expr])
    | ASymbol n ->
      let id = int_symbol ctx in 
      let expr = mk_symbol ctx ("A" ^ string_of_int n) (int_sort ctx) in
      (id, [mk_eq ctx id expr])
    (* Const *)
    | Int n -> 
      let id = int_symbol ctx in
      let expr = int_const ctx n in
      (id, [mk_eq ctx id expr])
    | Bool b -> 
      let id = bool_symbol ctx in
      let expr = bool_const ctx b in
      (id, [mk_eq ctx id expr])
    | Str str -> 
      let id = string_symbol ctx in
      let expr = str_const ctx str in
      (id, [mk_eq ctx id expr])
    | List svs ->
      begin match svs with
      | [] -> 
        let sort = int_sort ctx in (* Consider an arbitrary list as an int list *) 
        let id = list_symbol ctx sort in 
        let list_sort = list_sort ctx (sort_to_symbol sort) sort in
        let expr = mk_nil list_sort in
        (id, [mk_eq ctx id expr]) 
      | hd::tl -> translate ctor_map ctx (Cons (hd, List tl)) (* Convert a list to list_cons *)
      end
    | Tuple svs ->
      let (ids, eqns) = List.fold_left (
        fun (ids, eqns) sv ->
          let (new_id, eqn) = translate ctor_map ctx sv in
          (new_id::ids, eqn@eqns)
      ) ([], []) svs
      in
      let (ids, eqns) = (List.rev ids, List.rev eqns) in
      let sorts = List.map get_expr_sort ids in
      (* gen tuple *)
      let tuple_name = Z3.Symbol.mk_string ctx (List.fold_left (fun acc sort -> acc ^ "," ^ Z3.Sort.to_string sort) "" sorts) in
      let sort = tuple_sort ctx tuple_name sorts in 
      let id = tuple_symbol ctx sort in
      let expr = mk_tuple ctx sort ids in
      (id, (mk_eq ctx id expr)::eqns)
    | Ctor (x, svs) ->
      let sort = BatMap.find x ctor_map in
      let decls = Z3.Datatype.get_constructors sort in
      let ctor_func = List.find (fun decl ->
        let name = Z3.Symbol.to_string (Z3.FuncDecl.get_name decl) in
        x = name
      ) decls
      in
      (* evaluate parameters *)
      if svs = [] then
        let id = ctor_symbol ctx sort in
        let expr = ctor_app ctx ctor_func [] in
        (id, [mk_eq ctx id expr])
      else
        let svs = 
          begin match List.hd svs with
          | Tuple svs -> svs
          | x -> [x]
          end
        in
        let (ids, eqns) = List.fold_left (
          fun (ids, eqns) sv ->
            let (new_id, eqn) = translate ctor_map ctx sv in
            (new_id::ids, eqn@eqns)
        ) ([], []) svs
        in
        let (ids, eqns) = (List.rev ids, List.rev eqns) in
        let id = ctor_symbol ctx sort in
        let expr = ctor_app ctx ctor_func ids in
        (id, (mk_eq ctx id expr)::eqns)
    (* unary operation *)
    | Minus sv ->
      let id = int_symbol ctx in
      let (expr1, eqns) = translate ctor_map ctx sv in
      let expr = mk_minus ctx expr1 in
      (id, (mk_eq ctx id expr)::eqns)
    | Not sv ->
      let id = bool_symbol ctx in
      let (expr1, eqns) = translate ctor_map ctx sv in
      let expr = mk_not ctx expr1 in
      (id, (mk_eq ctx id expr)::eqns)
    (* binary operation *)
    | Aop (op, sv1, sv2) ->
      let id = int_symbol ctx in
      let (expr1, eqns1) = translate ctor_map ctx sv1 in
      let (expr2, eqns2) = translate ctor_map ctx sv2 in
      let expr = 
        begin match op with
        | Add -> mk_add ctx expr1 expr2
        | Sub -> mk_sub ctx expr1 expr2
        | Mul -> mk_mul ctx expr1 expr2
        | Div -> mk_div ctx expr1 expr2
        | Mod -> mk_mod ctx expr1 expr2
        end
      in
      (id, (mk_eq ctx id expr)::(eqns1@eqns2))
    | Bop (op, sv1, sv2) ->
      let id = bool_symbol ctx in
      let (expr1, eqns1) = translate ctor_map ctx sv1 in
      let (expr2, eqns2) = translate ctor_map ctx sv2 in
      let expr = 
        begin match op with
        | And -> mk_and ctx expr1 expr2
        | Or -> mk_or ctx expr1 expr2
        end
      in
      (id, (mk_eq ctx id expr)::(eqns1@eqns2))
    | ABop (op, sv1, sv2) ->
      let id = bool_symbol ctx in
      let (expr1, eqns1) = translate ctor_map ctx sv1 in
      let (expr2, eqns2) = translate ctor_map ctx sv2 in
      let expr = 
        begin match op with
        | Lt -> mk_lt ctx expr1 expr2
        | Gt -> mk_gt ctx expr1 expr2
        | Le -> mk_le ctx expr1 expr2
        | Ge -> mk_ge ctx expr1 expr2
        end
      in
      (id, (mk_eq ctx id expr)::(eqns1@eqns2))
    | EQop (op, sv1, sv2) ->
      let id = bool_symbol ctx in
      let (expr1, eqns1) = translate ctor_map ctx sv1 in
      let (expr2, eqns2) = translate ctor_map ctx sv2 in
      let expr = 
        begin match op with
        | Eq -> mk_eq ctx expr1 expr2
        | NEq -> mk_neq ctx expr1 expr2
        end
      in
      (id, (mk_eq ctx id expr)::(eqns1@eqns2))
    | Cons (sv1, sv2) ->
      begin match sv2 with
      | List [] ->
        (* hd *)
        let (expr1, eqns1) = translate ctor_map ctx sv1 in
        let sort = get_expr_sort expr1 in
        let list_sort = list_sort ctx (sort_to_symbol sort) sort in
        (* tail *)
        let id2 = list_symbol ctx sort in
        let expr2 = mk_nil list_sort in
        let eqns = eqns1@[mk_eq ctx id2 expr2] in
        (* result *)
        let id = list_symbol ctx sort in
        let expr = mk_cons ctx list_sort expr1 expr2 in
        (id, (mk_eq ctx id expr)::eqns)
      | _ ->
        let (expr1, eqns1) = translate ctor_map ctx sv1 in
        let (expr2, eqns2) = translate ctor_map ctx sv2 in
        let sort = get_expr_sort expr1 in
        let list_sort = list_sort ctx (sort_to_symbol sort) sort in
        let id = list_symbol ctx sort in
        let expr = mk_cons ctx list_sort expr1 expr2 in
        (id, (mk_eq ctx id expr)::(eqns1@eqns2))
      end
    | Strcon (sv1, sv2) ->
      let id = string_symbol ctx in
      let (expr1, eqns1) = translate ctor_map ctx sv1 in
      let (expr2, eqns2) = translate ctor_map ctx sv2 in
      let expr = mk_concat ctx expr1 expr2 in
      (id, (mk_eq ctx id expr)::(eqns1@eqns2))
    (* Invalid *)
    | Append (_, _) -> raise (Failure ("Translation is not implemented"))
    | Unit | Exn | Fun _ | FunRec _ | FunBlock _ -> raise (Failure ("Invalid Z3 input"))
  
  let translate_formula : ctor_map -> Z3.context -> vc_formula -> (Z3.Expr.expr * Z3.Expr.expr list)
  = fun ctor_map ctx f ->
    let id = bool_symbol ctx in
    let (ids, eqns) = List.fold_left (fun (ids, eqns) (pc, sv) ->
      let id = bool_symbol ctx in
      let (id1, eqns1) = translate ctor_map ctx pc in
      let (id2, eqns2) = translate ctor_map ctx sv in
      let expr = mk_imply ctx id1 id2 in
      (id::ids, (mk_eq ctx id expr)::(eqns1@eqns2@eqns))
    ) ([], []) f 
    in
    let expr = Z3.Boolean.mk_or ctx ids in
    (id, (mk_eq ctx id expr)::eqns)

  let translate_vc : ctor_map -> Z3.context -> vc -> (Z3.Expr.expr * Z3.Expr.expr list)
  = fun ctor_map ctx vc ->
    let id = bool_symbol ctx in
    let (ids, eqns) = List.fold_left (fun (ids, eqns) f ->
      let (id, eqn) = translate_formula ctor_map ctx f in
      (id::ids, eqn@eqns)
    ) ([], []) vc
    in
    let expr = Z3.Boolean.mk_and ctx ids in
    (id, (mk_eq ctx id expr)::eqns)
end

let is_valid : CtorTable.t -> vc_formula -> Z3.Model.model option
= fun ctor_table vc ->
  let ctx = Z3_Translator.new_ctx () in
  let ctor_map = Z3_Translator.init_ctor_map ctx ctor_table BatMap.empty in
  let ctor_map = Z3_Translator.apply_recursive_datatype ctx ctor_table ctor_map in
  let solver = Z3.Solver.mk_solver ctx None in
  let (id, eqns) = Z3_Translator.translate_formula ctor_map ctx vc in
  let eqns = (Z3_Translator.mk_eq ctx id (Z3_Translator.bool_const ctx true))::eqns in
  let _ = Z3.Solver.add solver eqns in
  (*
  print_endline ("*************");
  print_endline (List.fold_left (fun str (pc, sv) -> symbol_to_string pc ^ " => " ^ symbol_to_string sv ^ "\\/\n" ^ str) "" vc);
  *)
  (*print_endline (Z3.Solver.to_string solver);*)
  match (Z3.Solver.check solver []) with
  | UNSATISFIABLE -> None
  | UNKNOWN -> None
  | SATISFIABLE -> 
    (* There exists an counter example *)
    begin match Z3.Solver.get_model solver with
    | Some model -> Some model
    | None -> None
    end

let check : CtorTable.t -> vc_formula -> Z3.Model.model option
= fun ctor_table vc ->
  let ctx = Z3_Translator.new_ctx () in
  let ctor_map = Z3_Translator.init_ctor_map ctx ctor_table BatMap.empty in
  let ctor_map = Z3_Translator.apply_recursive_datatype ctx ctor_table ctor_map in
  let solver = Z3.Solver.mk_solver ctx None in
  let (id, eqns) = Z3_Translator.translate_formula ctor_map ctx vc in
  let eqns = (Z3_Translator.mk_not ctx id)::eqns in
  let _ = Z3.Solver.add solver eqns in
  (*print_endline (Z3.Solver.to_string solver);*)
  match (Z3.Solver.check solver []) with
  | UNSATISFIABLE -> (*print_endline ("SAT");*) None
  | UNKNOWN -> (*print_endline ("UNKNOWN");*) None
  | SATISFIABLE -> 
    (* There exists an counter example *)
    begin match Z3.Solver.get_model solver with
    | Some model -> (*print_endline (Z3.Model.to_string model);*) Some model
    | None -> (*print_endline ("SAT");*) None
    end

let check2 : CtorTable.t -> vc -> Z3.Model.model option
= fun ctor_table vc ->
  let ctx = Z3_Translator.new_ctx () in
  let ctor_map = Z3_Translator.init_ctor_map ctx ctor_table BatMap.empty in
  let ctor_map = Z3_Translator.apply_recursive_datatype ctx ctor_table ctor_map in
  let solver = Z3.Solver.mk_solver ctx None in
  let (id, eqns) = Z3_Translator.translate_vc ctor_map ctx vc in
  let eqns = (Z3_Translator.mk_not ctx id)::eqns in
  let _ = Z3.Solver.add solver eqns in
  (*print_endline (Z3.Solver.to_string solver);*)
  match (Z3.Solver.check solver []) with
  | UNSATISFIABLE -> (*print_endline ("SAT");*) None
  | UNKNOWN -> (*print_endline ("UNKNOWN");*) None
  | SATISFIABLE -> 
    (* There exists an counter example *)
    begin match Z3.Solver.get_model solver with
    | Some model -> (*print_endline (Z3.Model.to_string model);*) Some model
    | None -> (*print_endline ("SAT");*) None
    end
