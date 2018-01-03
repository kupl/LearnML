open Lang
open Util

type typ_eqn = (typ * typ) list
type hole_env = (int, id) BatMap.t
type hole_table = (int, typ) BatMap.t 

exception TypeError

let start_time = ref 0.0
let global_pgm = ref []


let gen_eqn_time = ref 0.0
let solve_time = ref 0.0

let hole_map = ref BatMap.empty
let hole_tbl = ref BatMap.empty


let rec string_of_type : typ -> string
=fun ty -> 
  match ty with
  | TInt -> "int"
  | TString -> "string"
  | TBool -> "bool"
  | TPoly -> "poly"
  | TBase id -> id
  | TList t -> string_of_type t ^ "list"
  | TTuple l -> string_of_type_list l
  | TCtor (t, l) -> string_of_type t ^ string_of_type_list l
  | TArr (t1,t2) -> "(" ^ string_of_type t1 ^ " -> " ^ string_of_type t2 ^ ")"
  | TVar x -> x

and string_of_type_list : typ list -> string
=fun tl ->
  "(" ^
  match tl with
  | [] -> ")"
  | hd::tl -> string_of_type hd ^ ", " ^ string_of_type_list tl

let print_typ_eqns eqns = 
  List.iter (fun (ty1,ty2) -> print_endline (string_of_type ty1 ^ " = " ^ string_of_type ty2)) eqns;
  print_endline ""

let rec print_hole_table tbl =
  if(BatMap.is_empty tbl) then ()
  else
    let ((n,typ),remain) = BatMap.pop tbl in
    print_endline("Hole "^ (string_of_int n) ^":" ^ string_of_type typ); print_hole_table remain


(* type environment : var -> type *)
module TEnv = struct
  type t = (id, typ) BatMap.t
  let empty = BatMap.empty
  let extend (x,t) tenv = BatMap.add x t tenv
  let find tenv x = BatMap.find x tenv
  let rec print tenv =
    if(BatMap.is_empty tenv) then ()
    else
      let ((id,typ),remain) = BatMap.pop tenv in
      print_endline(id ^ "|->" ^ (string_of_type typ) ^"\n" );
      print remain
end

type at_hole_env = (int, TEnv.t) BatMap.t

let at_hole_table = ref BatMap.empty
let at_hole_ttbl = ref BatMap.empty

let rec print_at_hole_table ttbl =
  BatMap.iter (fun n env -> let _ = print_endline(string_of_int n) in TEnv.print env) ttbl

(* substitution *)
module Subst = struct
  type t = (id * typ) list
  let empty = []
  let find x subst = List.assoc x subst

  (* walk through the type, replacing each type variable by its binding in the substitution *)
  let rec apply : typ -> t -> typ
  =fun typ subst ->
    match typ with
    | TInt -> TInt
    | TBool -> TBool 
    | TPoly -> TPoly
    | TString -> TString
    | TBase id -> TBase id
    | TList t -> TList (apply t subst)
    | TTuple l ->  TTuple (apply_to_list l subst)
    | TCtor (t1, l) -> TCtor (apply t1 subst, apply_to_list l subst)
    | TArr (t1,t2) -> TArr (apply t1 subst, apply t2 subst)
    | TVar x -> 
      try find x subst
      with _ -> typ

  and apply_to_list : typ list -> t -> typ list
  = fun l subst ->
    (*List.map??*)
    match l with
    |[] -> []
    |hd::tl -> (apply hd subst)::(apply_to_list tl subst)

  (* add a binding (tv,ty) to the subsutition and propagate the information *)
  let extend tv ty subst = 
    (tv,ty) :: (List.map (fun (x,t) -> (x, apply t [(tv,ty)])) subst)

  let print : t -> unit
  =fun subst -> 
      List.iter (fun (x,ty) -> print_endline (x ^ " |-> " ^ string_of_type ty)) subst
end

let tvar_num = ref 0


(* generate a fresh type variable *)
let fresh_tvar () = (tvar_num := !tvar_num + 1; (TVar ("#" ^ string_of_int !tvar_num)))


let rec type_remove_tpoly typ = 
  match typ with 
  | TList t -> TList (type_remove_tpoly t)
  | TTuple l -> TTuple(type_list_remove_tpoly l)
  | TCtor (t, l) -> TCtor (type_remove_tpoly t, type_list_remove_tpoly l)
  | TArr (t1,t2) -> TArr (type_remove_tpoly t1,type_remove_tpoly t2)
  | TPoly -> fresh_tvar ()
  |_ -> typ

and type_list_remove_tpoly l =
  match l with
  |[] -> []
  |hd::tl -> (type_remove_tpoly hd) :: (type_list_remove_tpoly tl)

let rec table_remove_tpoly hole_table result_map = 
  if(BatMap.is_empty hole_table) then result_map
  else
    let ((n,typ),remain)= BatMap.pop hole_table in
    table_remove_tpoly remain (BatMap.add n (type_remove_tpoly typ) result_map)

let rec hole_env_remove_tpoly hole_env result_map=
  if(BatMap.is_empty hole_env) then result_map
  else
    let ((n,env),remain) = BatMap.pop hole_env in
    let new_env = env_remove_tpoly env in
    hole_env_remove_tpoly remain (BatMap.add n new_env result_map)

and env_remove_tpoly env =
  if(BatMap.is_empty env) then BatMap.empty
  else
    let ((id,typ),remain) = BatMap.pop env in
    let typ = type_remove_tpoly typ in
    BatMap.add id typ (env_remove_tpoly remain)

(*********************)
(* Utility functions *)
(*********************)

let arg_to_typ (arg_id,arg_typ) = 
  match arg_typ with
  |TPoly -> fresh_tvar()
  |_ -> arg_typ 

let arg_to_id (arg_id,arg_typ) = arg_id 

let rec body_of_arr : typ -> typ
=fun tarr ->
  match tarr with
  |TArr (t1, t2) -> body_of_arr t2
  |_ -> tarr
  
let ctor_to_typ (tbase, tl) = tl

let ctor_to_id (tbase, tl) = tbase

let rec args_to_env args tenv =
  match args with
  |[] -> tenv
  |hd::tl -> args_to_env tl (TEnv.extend (arg_to_id hd, arg_to_typ hd) tenv)

let rec args_to_arr args ty tenv =
  match args with
  |[] -> (ty,tenv)
  |hd::tl -> 
    let arg_typ = arg_to_typ hd in
    let (args_typ,tenv) = (args_to_arr tl ty tenv) in
    ((TArr (arg_typ,args_typ)),(TEnv.extend (arg_to_id hd, arg_typ) tenv))

let rec tuple_to_eqn : exp list -> TEnv.t -> typ list -> typ_eqn -> (typ list * typ_eqn)
=fun el tenv tl eqns ->
  match el with
  |[] -> (List.rev tl,eqns)
  |e::etl -> 
    let t1 = fresh_tvar() in
    tuple_to_eqn etl tenv (t1::tl) ((gen_equations tenv e t1)@eqns)

and ctors_to_eqn : exp list -> typ list -> TEnv.t -> typ_eqn -> typ_eqn
=fun el tl tenv eqns ->
  match (el, tl) with
  |([],[]) -> eqns
  |(e::etl ,t::ttl) ->
    ctors_to_eqn etl ttl tenv ((gen_equations tenv e t)@eqns)
  |_ -> raise TypeError

(*******************************************)
(*type inference rule of pattern expression*)
(*******************************************)

and pctor_to_eqn : pat list -> typ list -> TEnv.t -> typ_eqn -> (typ_eqn * TEnv.t)
=fun pl tl tenv eqns->
  match (pl, tl) with
  |([],[]) -> (eqns, tenv)
  |(p::ptl ,t::ttl) ->
    let (new_eqn, new_env) = pat_to_eqn p t tenv in
    pctor_to_eqn ptl ttl new_env (new_eqn@eqns)
  |_ -> raise TypeError

and ptuple_to_eqn : pat list -> typ list -> TEnv.t -> typ_eqn -> (typ_eqn * TEnv.t * typ list)
=fun pl tl tenv eqns->
  match pl with
  |[] -> (eqns, tenv, List.rev tl)
  |p::ptl ->
    let t1 = fresh_tvar() in
    let (new_eqn, new_env) = pat_to_eqn p t1 tenv in
    ptuple_to_eqn ptl (t1::tl) new_env (new_eqn@eqns)

and cons_to_eqn : pat list -> typ -> TEnv.t -> typ_eqn -> (typ_eqn * TEnv.t)
=fun pl ty tenv eqns->
  (*pl has two or more elements, its last element is list*)
  match pl with
  |[p] -> 
    let (new_eqn, new_env) = pat_to_eqn p (TList ty) tenv in
    (new_eqn@eqns, new_env)
  |hd::tl ->
    let (new_eqn, new_env) = pat_to_eqn hd ty tenv in
    cons_to_eqn tl ty new_env (new_eqn@eqns)

(*check the type of patterns and bind variables in patterns to env*)
and pat_to_eqn : pat -> typ -> TEnv.t -> (typ_eqn * TEnv.t)
=fun p ty tenv ->
  match p with
  | PInt n  -> ([(ty, TInt)], tenv)
  | PBool b -> ([(ty, TBool)], tenv)
  | PVar x-> 
    let t1 = fresh_tvar() in
    ([ty, t1], (TEnv.extend (x, t1) tenv))
  | PUnder -> ([ty, TPoly], tenv)
  | PCtor (id, pl) -> 
    let tctor = TEnv.find tenv id in
    begin match tctor with
    | TCtor (tbase, tlist) ->
      let (new_eqn, new_env) = pctor_to_eqn pl tlist tenv [] in
      ([ty, tbase]@new_eqn, new_env)
    |_ -> raise TypeError
    end
  | PTuple pl -> 
    begin match pl with
    |[] -> ([(ty, TTuple [])], tenv)
    |hd::tl -> 
      let (new_eqn, new_env, typ_list) = ptuple_to_eqn pl [] tenv [] in
      (([ty, TTuple (typ_list)]@new_eqn), new_env)
    end
  | PList pl -> 
    begin match pl with
    |[] -> 
      let t1 = fresh_tvar() in
      ([(ty, TList t1)], tenv)
    |hd::tl ->
      let t1 = fresh_tvar() in
      let (eqn1, env1) = pat_to_eqn hd t1 tenv in
      let (eqn2, env2) = pat_to_eqn (PList tl) (TList t1) env1 in
      (([ty, TList t1]@eqn1@eqn2), env2)
    end
  | PCons pl ->
    let t1 = fresh_tvar() in
    let (new_eqn, new_env) = cons_to_eqn pl t1 tenv [] in
    (([ty, TList t1]@new_eqn), new_env)
  | Pats pl -> 
    begin match pl with
    |[] -> ([], tenv)
    |hd::tl -> 
      let (eqn1, env1) = pat_to_eqn hd ty tenv in
      let (eqn2, env2) = pat_to_eqn (Pats tl) ty env1 in
      ((eqn1@eqn2), env2)
    end
      
(*make type equations of branch*)
and branch_to_eqn : branch list -> typ -> typ -> TEnv.t -> typ_eqn
=fun bs texp tpat tenv->
  match bs with
  |[] -> []
  |(p, e)::tl ->
    let (new_eqn, new_env) = pat_to_eqn p tpat tenv in
    new_eqn@((gen_equations new_env e texp)@(branch_to_eqn tl texp tpat tenv))

and gen_equations : TEnv.t -> exp -> typ -> typ_eqn 
=fun tenv e ty ->
  if(Sys.time()-. !start_time > 0.1) then 
    (
    (*print_endline("-----------gen----------");
    print_endline(Print.program_to_string !global_pgm);
    print_endline("---------------------");
    exit 0;*)
    raise TypeError)
  else
  match e with 
    (*base*)
    | Const n -> (ty, TInt)::[]
    | EVar x -> (ty, (TEnv.find tenv x))::[]
    | String x -> (ty, TString)::[]
    | EList l ->
    begin match l with
      |[] -> 
        let t1 = fresh_tvar () in
        (ty, TList t1)::[]
      |hd::tl ->
        let t1 = fresh_tvar() in
        (ty, TList t1)::((gen_equations tenv hd t1)@(gen_equations tenv (EList tl) (TList t1)))
    end
    | ETuple l ->
    begin match l with
      |[] -> (ty, TTuple [])::[]
      |hd::tl -> 
        let (typ_list, new_eqn) = tuple_to_eqn l tenv [] [] in
        (ty, TTuple typ_list)::new_eqn
    end
    | ECtor (id, l) -> 
      let tctor = TEnv.find tenv id in
      begin match tctor with
      | TCtor (tbase, tl) -> (ty, tbase)::(ctors_to_eqn l tl tenv [])
      | _ -> raise TypeError
      end
    (*aexp*)
    | ADD (e1, e2) 
    | SUB (e1, e2) 
    | MUL (e1, e2)
    | DIV (e1, e2) 
    | MOD (e1, e2)-> (ty, TInt)::((gen_equations tenv e1 TInt)@(gen_equations tenv e2 TInt))
    | MINUS e -> (ty, TInt)::(gen_equations tenv e TInt)
    (*bexp*)
    | TRUE
    | FALSE -> (ty,TBool)::[]
    | NOT e1 -> (ty,TBool)::(gen_equations tenv e1 TBool)
    | OR (e1, e2)
    | AND (e1, e2) -> (ty, TBool)::((gen_equations tenv e1 TBool)@(gen_equations tenv e2 TBool))
    | LESS (e1, e2)
    | LARGER (e1, e2)
    | LESSEQ (e1, e2)
    | LARGEREQ (e1, e2) -> (ty, TBool)::((gen_equations tenv e1 TInt)@(gen_equations tenv e2 TInt))
    | EQUAL (e1, e2)
    | NOTEQ (e1, e2) -> 
      let t1 = fresh_tvar() in
      (ty, TBool) :: ((gen_equations tenv e1 t1)@(gen_equations tenv e2 t1))
    (* lexp *)
    | AT (e1, e2) ->
      let t1 = fresh_tvar() in
      (ty, t1) :: ((gen_equations tenv e1 t1)@(gen_equations tenv e2 t1))
    | DOUBLECOLON (e1, e2) ->
      let t1 = fresh_tvar() in
      (ty, TList t1) :: ((gen_equations tenv e1 t1)@(gen_equations tenv e2 (TList t1)))
    (* else *)
    | IF (e1, e2 ,e3) ->
      (gen_equations tenv e1 TBool)@(gen_equations tenv e2 ty)@(gen_equations tenv e3 ty)
    | ELet (f, is_rec, args, typ, e1, e2) ->
      (*t1 = type of function body (e1) *)
      let t1 = if typ = TPoly then fresh_tvar() else body_of_arr typ in
      begin match args with
        | [] -> 
          (gen_equations tenv e1 t1)@(gen_equations (TEnv.extend (f, t1) tenv) e2 ty)
        | _ ->
          let (args_ty,new_env) = args_to_arr args t1 tenv in (* args : ty1 -> ty2 -> ty3..*)
          (*let new_env = args_to_env args tenv in*)
          let func_type = if typ = TPoly then args_ty else typ in
          (gen_equations (TEnv.extend (f, func_type) new_env) e1 t1)@
          (gen_equations (TEnv.extend (f, func_type) tenv) e2 ty)
      end
    | EMatch (e, bs) ->
      let t1 = fresh_tvar() in (*type of expression*)
      let t2 = fresh_tvar() in (*type of pattern*)
      (ty, t1)::(gen_equations tenv e t2)@(branch_to_eqn bs t1 t2 tenv)
    | EFun (arg, e) ->
      let t1= arg_to_typ arg in 
      let x= arg_to_id arg in
      let t2= fresh_tvar() in
      (ty, TArr(t1,t2)) :: (gen_equations (TEnv.extend (x, t1) tenv) e t2)
    | EApp (e1, e2) ->
      let t1 = fresh_tvar() in
      (gen_equations tenv e1 (TArr (t1, ty)))@(gen_equations tenv e2 t1)
    | Hole n -> 
      let t1 = fresh_tvar() in
      let _ = hole_map := BatMap.add n t1 (!hole_map) in
      let _ = at_hole_table := BatMap.add n tenv (!at_hole_table) in
      (ty, t1)::(t1,TPoly)::[]

let rec extract_tvar : id -> typ -> bool
= fun x t ->
  match t with
  |TList t -> extract_tvar x t
  |TTuple l 
  |TCtor (_, l) -> extract_tvar2 x l
  |TArr (t1, t2) -> (extract_tvar x t1) || (extract_tvar x t2)
  |TVar y -> (x=y)
  |_ -> false

and extract_tvar2 : id -> typ list -> bool
= fun x lst -> List.exists (fun t -> extract_tvar x t) lst

let rec unify : typ -> typ -> Subst.t -> Subst.t
= fun typ1 typ2 subst ->
  if(Sys.time()-. !start_time > 0.1) then 
    (
    (*print_endline("----------unify-----------");
    print_endline(Print.program_to_string !global_pgm);
    Subst.print subst;
    print_endline("---------------------");
    exit 0;*)
    raise TypeError)
  else
  match (typ1, typ2) with
  |(TInt, TInt) -> subst
  |(TBool, TBool) -> subst
  |(TString, TString) -> subst
  |(typ, TPoly) -> unify TPoly typ subst
  |(TPoly, typ) -> subst (*??*)
  |(TBase id1, TBase id2) -> if id1 = id2 then subst else raise TypeError
  |(TList t1, TList t2) -> unify t1 t2 subst
  |(TTuple tl1, TTuple tl2) -> unify_list tl1 tl2 subst
  |(TCtor (id1, tl1), TCtor (id2, tl2)) ->
    if id1 <> id2 then raise TypeError
    else unify_list tl1 tl2 subst
  |(TArr(t1, t2), TArr(t1',t2')) ->
    let subst' = unify t1 t1' subst in
    let t3 = Subst.apply t2 subst' in
    let t4 = Subst.apply t2' subst' in
      unify t3 t4 subst'
  |(TVar x, typ) -> 
    (*if x occurs in typ, raise type error*)
    begin
      match typ with 
      |TVar y -> 
        if x = y then subst else Subst.extend x typ subst
      |TList t1 ->
        if extract_tvar x t1 then raise TypeError
        else Subst.extend x typ subst
      |TTuple tl -> 
        if extract_tvar2 x tl then raise TypeError
        else Subst.extend x typ subst
      |TCtor (id, tl) ->
        if extract_tvar2 x tl then raise TypeError
        else Subst.extend x typ subst
      |TArr (t1, t2) ->
        if ((extract_tvar x t1) || (extract_tvar x t2)) then raise TypeError
        else Subst.extend x typ subst
      |_ -> Subst.extend x typ subst
    end
  |(typ, TVar x) -> unify (TVar x) typ subst
  |(_, _) -> raise TypeError

and unify_list : typ list -> typ list -> Subst.t -> Subst.t
= fun l1 l2 subst ->
  if (List.length l1) <> (List.length l2) then raise TypeError else 
  begin match (l1, l2) with
  | ([], []) -> subst
  | ((hd1::tl1),(hd2::tl2)) -> unify_list tl1 tl2 (unify hd1 hd2 subst)
  end

let rec unify_all : typ_eqn -> Subst.t -> Subst.t
= fun eqns subst ->
  if(Sys.time()-. !start_time > 1.0) then 
  (
    (*print_endline("-----------unify_all----------");
    print_endline(Print.program_to_string !global_pgm);
    print_endline("---------------------");*)
    raise TypeError
  )
  else
  match eqns with
  |[] -> subst
  |(typ1, typ2)::tl ->
    (*let _ = print_endline((Print.string_of_type typ1) ^ " = " ^ (Print.string_of_type typ2)) in*)
    let subst' = unify (Subst.apply typ1 subst) (Subst.apply typ2 subst) subst in
    unify_all tl subst'
  
let solve : typ_eqn -> Subst.t
= fun eqns -> 
  unify_all eqns Subst.empty

let rec fill_hole_tbl subst holes= 
  if(BatMap.is_empty holes) then ()
  else
    let ((n,id),remain) = BatMap.pop holes in
    let ty = Subst.apply id subst in
    
    (*Subst.print subst;
    
    print_endline(string_of_type id);
    print_endline(string_of_type ty);
    *)
    try 
      let _ = BatMap.find n !hole_tbl in
      fill_hole_tbl subst remain
    with _ ->
      let _ = hole_tbl := BatMap.add n ty !hole_tbl in
      fill_hole_tbl subst remain

let rec complete_tenv subst env_table =
  if(BatMap.is_empty env_table) then ()
  else
    let ((n,env),remain) = BatMap.pop env_table in
    let rec change_env env result_env=
      if(BatMap.is_empty env) then result_env
      else 
        let ((id,t),remain) = BatMap.pop env in
        let result_env = BatMap.add id (Subst.apply t subst) result_env in
        change_env remain result_env
    in
    let result_env = change_env env BatMap.empty in
    let _ = at_hole_ttbl:= BatMap.add n result_env (!at_hole_ttbl) in
    complete_tenv subst remain

let typeof : exp -> TEnv.t -> typ -> typ
=fun exp tenv typ->
(*  let _ = print_endline(Print.exp_to_string exp) in*)
  let new_tv = fresh_tvar () in
  let eqns = gen_equations tenv exp new_tv in
  let eqns = if(typ=TPoly) then eqns else (new_tv,typ)::eqns in
  (*let _ = print_endline "= Equations = ";
          print_typ_eqns eqns;
          print_endline "" in *)
  let subst = solve eqns in
 (* let _ = print_endline("---------------") in
  let _ = Subst.print subst in
  let _ = print_endline("---------------") in*)
  let ty = Subst.apply new_tv subst in
  let _ = complete_tenv subst !at_hole_table in
  let _ = fill_hole_tbl subst !hole_map in
    (*print_endline "= Substitution = ";
    Subst.print subst;
    print_endline "";
    print_endline ("Type: " ^ string_of_type ty);
    print_endline ""; *)
    ty

let rec ctors_to_env : ctor list -> TEnv.t -> typ -> TEnv.t
=fun ctors tenv tbase ->
  match ctors with
  | [] -> tenv
  | (id,typ_lst)::tl -> 
    let ty = TCtor (tbase, typ_lst) in
    ctors_to_env tl (TEnv.extend (id, ty) tenv) tbase

let type_decl : decl -> TEnv.t -> TEnv.t
=fun decl tenv -> 
  at_hole_table:=BatMap.empty;
  match decl with
  | DData (id, ctors) -> 
    let tbase = TBase id in
    ctors_to_env ctors tenv tbase
  | DLet (x,is_rec,args,typ,exp) -> 
    begin match args with
    | [] -> (* variable binding *)
      let ty = typeof exp (TEnv.extend (x, typ) tenv) typ in
      begin match typ with
      | TPoly -> TEnv.extend (x, ty) tenv
      | _ -> if ty = typ then TEnv.extend (x, ty) tenv else raise TypeError
      end
    | _ ->  (* function binding *)
      let e = ELet(x, is_rec, args, typ, exp, EVar(x)) in
      let ty = typeof e (TEnv.extend (x, typ) tenv) typ in
      begin match typ with
      | TPoly -> TEnv.extend (x, ty) tenv
      | _ -> if ty = typ then TEnv.extend (x, ty) tenv else raise TypeError
      end
    end

let type_time = ref 0.0

let run : prog -> prog
=fun decls -> 
(*  let _ = print_endline("----------------------") in*)
  let _ = hole_map:=BatMap.empty in
  let _ = hole_tbl:=BatMap.empty in
  let _ = at_hole_ttbl:=BatMap.empty in
  let _ = start_time:=Sys.time() in
  let _ = tvar_num:=0 in
  let _ = global_pgm:=decls in
  let tenv = (list_fold type_decl decls TEnv.empty) in
  let _ = hole_tbl:=(table_remove_tpoly !hole_tbl BatMap.empty) in
  let _ = at_hole_ttbl:= (hole_env_remove_tpoly !at_hole_ttbl BatMap.empty) in
(*  let _ = TEnv.print tenv in*)
(*  let _ = print_endline(Print.program_to_string decls) in
  let _ = print_endline("----------------------") in
  let _ = print_hole_table !hole_tbl in
  let _ = print_endline("----------------------") in*)
  decls
