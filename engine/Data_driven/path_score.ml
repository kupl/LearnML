open Util
open Lang
open CallGraph
open Z3
open Z3.Solver

(* type constructor table (for constraint generation) *)
module CtorTable = struct
  (* 
    Type name => Constructors name => Constructor arugments 
    ex) exp -> [ADD => exp * exp | SUB => exp * exp | Int => int | X => .]
  *)
  type t = (id, (id, typ list) BatMap.t) BatMap.t

  let gen_ctor_table : t -> prog -> t
  = fun t decls ->
    let rec iter : t -> decl -> t
    = fun t decl ->
      match decl with
      | DData (x, ctors) -> 
        let ctors = List.fold_left (fun ctors (x, typs) -> 
          BatMap.add x typs ctors
        ) BatMap.empty ctors
        in
        BatMap.add x ctors t
      | TBlock decls -> List.fold_left iter t decls
      | _ -> t
    in
    let decls = Type.Converter.convert Type.Converter.empty decls in
    List.fold_left iter t decls
    
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
  (* 
    Mapping for User_defined_type_name -> Z3.Sort 
    ex)
      ADD => exp
      SUB => exp
      ...
  *)
  type ctor_map = (id, Z3.Sort.sort) BatMap.t

  let sort_to_symbol sort = Z3.Sort.get_name sort
  let counter = ref 0
  let new_counter () = (counter := !counter + 1); !counter
  (* context => if the formula is too hard to solve in 0.01 sec, fail to find a counter example *)
  let new_ctx () = mk_context [("timeout", "10"); ]

  (* sort *)
  let int_sort ctx = Z3.Arithmetic.Integer.mk_sort ctx
  let bool_sort ctx = Z3.Boolean.mk_sort ctx
  let string_sort ctx = Z3.Seq.mk_string_sort ctx
  let list_sort ctx symbol sort = Z3.Z3List.mk_sort ctx (Z3.Symbol.mk_string ctx ((Z3.Symbol.to_string symbol) ^ "_List")) sort
  let tuple_sort ctx symbol sorts = Z3.Tuple.mk_sort ctx symbol (List.map sort_to_symbol sorts) sorts
  let get_expr_sort expr = Z3.Expr.get_sort expr

  (* Init constructor info *)
  let rec typ_to_sort : ctor_map -> Z3.context -> typ -> Z3.Sort.sort
  = fun map ctx typ ->
    match typ with
    | TInt -> int_sort ctx
    | TBool -> bool_sort ctx
    | TString -> string_sort ctx
    | TBase x -> (try BatMap.find x map with Not_found -> Z3.Sort.mk_uninterpreted_s ctx ("Undef" ^ x)) (* Undefined *)
    | TList t -> 
      let sort = typ_to_sort map ctx t in
      list_sort ctx (sort_to_symbol sort) sort 
    | TTuple ts -> 
      let sorts = List.map (typ_to_sort map ctx) ts in
      let tuple_name = Z3.Symbol.mk_string ctx (List.fold_left (
        fun acc sort -> if acc = "" then Z3.Sort.to_string sort else acc ^ "," ^ Z3.Sort.to_string sort
      ) "" sorts) in
      tuple_sort ctx tuple_name sorts
    | TVar tx -> Z3.Sort.mk_uninterpreted_s ctx "poly" (* Polymorphic *)
    | TArr (t1, t2) -> Z3.Sort.mk_uninterpreted_s ctx (Print.type_to_string typ) (* Function *)
    | _ -> raise (Failure ("Fail to convert " ^ Print.type_to_string typ ^ " type to Z3 sort")) 

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
            ) ts
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

  (* Constant *)
  let int_const ctx n = Z3.Arithmetic.Integer.mk_numeral_i ctx n
  let bool_const ctx b = Z3.Boolean.mk_val ctx b
  let str_const ctx str = Z3.Seq.mk_string ctx str
  let mk_var ctx id sort = Z3.Expr.mk_const_s ctx id sort
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
  (* abop *)
  let mk_lt ctx expr1 expr2 = Z3.Arithmetic.mk_lt ctx expr1 expr2
  let mk_le ctx expr1 expr2 = Z3.Arithmetic.mk_le ctx expr1 expr2
  let mk_gt ctx expr1 expr2 = Z3.Arithmetic.mk_gt ctx expr1 expr2
  let mk_ge ctx expr1 expr2 = Z3.Arithmetic.mk_ge ctx expr1 expr2
  (* eq *)
  let mk_eq ctx expr1 expr2 = Z3.Boolean.mk_eq ctx expr1 expr2
  let mk_neq ctx expr1 expr2 = (mk_not ctx (mk_eq ctx expr1 expr2))
  (* list  *)
  let mk_nil sort = Z3.Z3List.nil sort
  let mk_cons ctx sort expr1 expr2 =
    let cons_func = Z3.Z3List.get_cons_decl sort in
    Z3.Expr.mk_app ctx cons_func [expr1; expr2]
  let mk_append ctx sort expr1 expr2 = 
    let app_func = Z3.FuncDecl.mk_func_decl ctx (Z3.Symbol.mk_string ctx "List.Append") [sort; sort] sort in
    Z3.Expr.mk_app ctx app_func [expr1; expr2]
  (* tuple *)
  let mk_tuple ctx sort exprs =
    let tuple_func = Z3.Tuple.get_mk_decl sort in
    Z3.Expr.mk_app ctx tuple_func exprs
  (* string *)
  let mk_concat ctx expr1 expr2 = Z3.Seq.mk_seq_concat ctx [expr1; expr2]
  (* constructor *)
  let ctor_app ctx ctor_func exprs = Z3.Expr.mk_app ctx ctor_func exprs
  (* Generate a symbolic variable whose type is sort in current context *)
  let symbol_num = ref 0
  let fresh_id () = (symbol_num := !symbol_num + 1); ("@" ^ (string_of_int !symbol_num))
  (* Symbolic variable (identifier) with type sort *)
  let mk_symbol ctx sort = Z3.Expr.mk_const_s ctx (fresh_id ()) sort
  let int_symbol ctx = mk_symbol ctx (int_sort ctx)
  let bool_symbol ctx = mk_symbol ctx (bool_sort ctx)
  let string_symbol ctx = mk_symbol ctx (string_sort ctx)

  (* Translate given path into an equation *)
  let rec translate : ctor_map -> Z3.context -> path -> (Z3.Expr.expr * Z3.Expr.expr list)
  = fun ctor_map ctx path ->
    match path with
    | Var (x, typ) ->
      let sort = typ_to_sort ctor_map ctx typ in
      let id = mk_symbol ctx sort in
      let expr = mk_var ctx x sort in
      (id, [mk_eq ctx id expr])
    | Symbol (n, typ) -> 
      let sort = typ_to_sort ctor_map ctx typ in
      let id = mk_symbol ctx sort in
      let expr = mk_var ctx ("?" ^ string_of_int n) sort in
      (id, [mk_eq ctx id expr])
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
    | List (ps, typ) ->
      begin match ps with
      | [] -> 
        let sort = typ_to_sort ctor_map ctx typ in
        let id = mk_symbol ctx sort in
        let expr = mk_nil sort in
        (id, [mk_eq ctx id expr])
      | hd::tl -> translate ctor_map ctx (Concat (hd, List (tl, typ))) (* Convert a list to list_cons *)
      end
    | Tuple ps ->
      let (ids, eqns) = List.fold_left (fun (ids, eqns) p ->
        let (new_id, eqn) = translate ctor_map ctx p in
        (new_id::ids, eqn@eqns)
      ) ([], []) ps
      in
      let (ids, eqns) = (List.rev ids, List.rev eqns) in
      let sorts = List.map get_expr_sort ids in
      (* gen tuple => specific datatype case *)
      let tuple_name = List.fold_left (fun acc sort -> 
        if acc = "" then Z3.Sort.to_string sort else acc ^ "," ^ Z3.Sort.to_string sort  
      ) "" sorts in
      let tuple_sort = Z3.Datatype.mk_sort_s ctx tuple_name 
        [(Z3.Datatype.mk_constructor_s ctx 
          "Tuple" 
          (Z3.Symbol.mk_string ctx "is_tuple")
          (List.map sort_to_symbol sorts)
          (List.map (fun sort -> Some sort) sorts)
          (List.map (fun sort -> 0) sorts))]
      in
      let decl = List.hd (Z3.Datatype.get_constructors tuple_sort) in
      let id = mk_symbol ctx tuple_sort in
      let expr = Z3.Expr.mk_app ctx decl ids in
      (id, (mk_eq ctx id expr)::eqns)
    | Ctor (c, ps) ->
      let sort = BatMap.find c ctor_map in
      let decls = Z3.Datatype.get_constructors sort in
      let ctor_func = List.find (fun decl ->
        let name = Z3.Symbol.to_string (Z3.FuncDecl.get_name decl) in
        c = name
      ) decls
      in
      (* evaluate parameters *)
      if ps = [] then
        let id = mk_symbol ctx sort in
        let expr = ctor_app ctx ctor_func [] in
        (id, [mk_eq ctx id expr])
      else
        let ps = (match List.hd ps with
          | Tuple ps -> ps
          | p -> [p]) in
        let (ids, eqns) = List.fold_left (fun (ids, eqns) p ->
          let (new_id, eqn) = translate ctor_map ctx p in
          (new_id::ids, eqn@eqns)
        ) ([], []) ps
        in
        let (ids, eqns) = (List.rev ids, List.rev eqns) in
        let id = mk_symbol ctx sort in
        let expr = ctor_app ctx ctor_func ids in
        (id, (mk_eq ctx id expr)::eqns)
    | Minus p ->
      let id = int_symbol ctx in
      let (expr1, eqns) = translate ctor_map ctx p in
      let expr = mk_minus ctx expr1 in
      (id, (mk_eq ctx id expr)::eqns)
    | Not p ->
      let id = bool_symbol ctx in 
      let (expr1, eqns) = translate ctor_map ctx p in
      let expr = mk_not ctx expr1 in
      (id, (mk_eq ctx id expr)::eqns)
    | Aop (op, p1, p2) ->
      let id = int_symbol ctx in
      let (expr1, eqns1) = translate ctor_map ctx p1 in
      let (expr2, eqns2) = translate ctor_map ctx p2 in
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
    | Bop (comp, p1, p2) ->
      let id = bool_symbol ctx in
      let (expr1, eqns1) = translate ctor_map ctx p1 in
      let (expr2, eqns2) = translate ctor_map ctx p2 in
      let expr = 
        begin match comp with
        | And -> mk_and ctx expr1 expr2
        | Or -> mk_or ctx expr1 expr2
        end
      in
      (id, (mk_eq ctx id expr)::(eqns1@eqns2))
    | ABop (comp, p1, p2) ->
      let id = bool_symbol ctx in
      let (expr1, eqns1) = translate ctor_map ctx p1 in
      let (expr2, eqns2) = translate ctor_map ctx p2 in
      let expr = 
        begin match comp with
        | Lt -> mk_lt ctx expr1 expr2
        | Gt -> mk_gt ctx expr1 expr2
        | Le -> mk_le ctx expr1 expr2
        | Ge -> mk_ge ctx expr1 expr2
        end
      in
      (id, (mk_eq ctx id expr)::(eqns1@eqns2))
    | EQop (eq, p1, p2) ->
      let id = bool_symbol ctx in
      let (expr1, eqns1) = translate ctor_map ctx p1 in
      let (expr2, eqns2) = translate ctor_map ctx p2 in
      let expr = 
        begin match eq with
        | Eq -> mk_eq ctx expr1 expr2
        | NEq -> mk_neq ctx expr1 expr2
        end
      in
      (id, (mk_eq ctx id expr)::(eqns1@eqns2))
    | Strcon (p1, p2) ->
      let id = string_symbol ctx in
      let (expr1, eqns1) = translate ctor_map ctx p1 in
      let (expr2, eqns2) = translate ctor_map ctx p2 in
      let expr = mk_concat ctx expr1 expr2 in
      (id, (mk_eq ctx id expr)::(eqns1@eqns2))
    | Concat (p1, p2) ->
      let (expr1, eqns1) = translate ctor_map ctx p1 in
      let (expr2, eqns2) = translate ctor_map ctx p2 in
      let list_sort = get_expr_sort expr2 in
      let id = mk_symbol ctx list_sort in
      let expr = mk_cons ctx list_sort expr1 expr2 in
      (id, (mk_eq ctx id expr)::(eqns1@eqns2))
    | Append (p1, p2) ->
      let (expr1, eqns1) = translate ctor_map ctx p1 in
      let (expr2, eqns2) = translate ctor_map ctx p2 in
      let list_sort = get_expr_sort expr2 in
      let id = mk_symbol ctx list_sort in
      let expr = mk_append ctx list_sort expr1 expr2 in
      (id, (mk_eq ctx id expr)::(eqns1@eqns2))
    | _ -> raise (Failure ("Fail to convert " ^ string_of_path path ^ " to a Z3 expression"))
 
  (* PP *)
  let print_formula : Z3.Expr.expr list -> unit
  = fun exprs -> List.iter (fun expr -> print_endline (Z3.Expr.to_string expr)) exprs

  let print_ctor_map : ctor_map -> unit
  = fun map -> BatMap.iter (fun id sort -> 
      let decls = Z3.Datatype.get_constructors sort in
      print_endline ("Decls : ");
      List.iter (fun decl -> print_endline (Z3.FuncDecl.to_string decl)) decls;
      print_endline (id ^ " -> " ^ Z3.Sort.to_string sort)
    ) map
end

let check_sat : CtorTable.t -> path -> bool
= fun ctor_table path ->
  try
    let ctx = Z3_Translator.new_ctx () in
    let ctor_map = Z3_Translator.init_ctor_map ctx ctor_table BatMap.empty in
    let ctor_map = Z3_Translator.apply_recursive_datatype ctx ctor_table ctor_map in
    let solver = Z3.Solver.mk_solver ctx None in
    let (id, eqns) = Z3_Translator.translate ctor_map ctx path in
    let eqns = (Z3_Translator.mk_eq ctx id (Z3.Boolean.mk_true ctx))::eqns in
    let _ = Z3.Solver.add solver eqns in
    match (Z3.Solver.check solver []) with
    | UNSATISFIABLE -> (* print_endline ("UNSAT"); *) false
    | UNKNOWN -> (* print_endline ("UNSAT"); *) false
    | SATISFIABLE -> (* print_endline ("SAT"); *) true
  with _ -> false