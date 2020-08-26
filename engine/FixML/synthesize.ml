open Lang
open Util
open Printf

(*
 ******************************************************
   Code for Synthesizing the "Hole"
 ******************************************************
*)

type type_env = Type.HoleType.t * Type.VariableType.t

type state = lexp * Type.HoleType.t * Type.VariableType.t

module Workset = struct
  type work = int * prog * Type.HoleType.t * Type.VariableType.t

  module OrderedType = struct
    type t = work
    let compare (rank1,p1,_,_) (rank2,p2,_,_) =
    let (c1,c2) = (rank1+(cost p1),rank2+(cost p2)) in
      if c1=c2 then 0 else
      if c1>c2 then 1
      else -1
  end

  module Heap = BatHeap.Make (OrderedType)

  (* type of workset : heap * (string set) *)
  type t = Heap.t * string BatSet.t
  let empty = (Heap.empty, BatSet.empty)

  let explored : prog -> t -> bool
  = fun pgm (_,sset) -> BatSet.mem (Print.program_to_string pgm) sset

  let add : work -> t -> t
  = fun (n,pgm,h_t,h_e) (heap,sset) ->
    try
      let normalized_pgm = Normalize.normalize pgm in
      if explored normalized_pgm (heap,sset) then (heap,sset)
      else
        (Heap.add (n,pgm,h_t,h_e) heap, BatSet.add (Print.program_to_string normalized_pgm) sset)
    with
      |_ -> (heap,sset)
  let choose : t -> (work * t) option
  = fun (heap,sset) ->
    try
      let elem = Heap.find_min heap in
      Some (elem, (Heap.del_min heap, sset))
    with
      | _ -> None

  let workset_info : t -> string
  = fun (heap,sset) ->
    "To explore : " ^ (string_of_int (Heap.size heap)) ^
    " Explored : " ^ (string_of_int (BatSet.cardinal sset))
end

let extract_holenum : lexp -> int 
= fun e -> 
  match snd e with
  |Hole n -> n
  |_ -> raise (Failure "error during obtain hole number")

let get_ctor_type : typ -> typ * typ list 
= fun typ -> 
  match typ with
  |TCtor (t,tl) -> (t,tl)
  |_ -> raise (Failure "Constructor type does not included at synthesizing")

(****************************************************)
(* Subtitution for type-directed enumerative search *)
(****************************************************)

let rec update_type : typ -> typ -> typ -> typ
= fun t tyvar typ -> 
  match t with
  |TVar id -> if(t=tyvar) then typ else t
  |TList ty -> TList (update_type ty tyvar typ)
  |TTuple lst -> TTuple (update_type_lst lst tyvar typ)
  |TCtor (ty,lst) -> TCtor (ty,update_type_lst lst tyvar typ)
  |TArr (ty1,ty2) -> TArr(update_type ty1 tyvar typ, update_type ty2 tyvar typ)
  |_ -> t

and update_type_lst : typ list -> typ -> typ -> typ list
= fun lst tyvar typ ->
  list_map (fun t -> update_type t tyvar typ) lst 

let update_env : typ -> typ -> Type.TEnv.t -> Type.TEnv.t
= fun tyvar typ env ->
  BatMap.map (fun t -> update_type t tyvar typ) env

let update_type_env : typ -> typ -> Type.VariableType.t -> Type.VariableType.t
= fun tyvar typ variable_types ->
  BatMap.map (fun tenv -> update_env tyvar typ tenv) variable_types

let update_hole_type : typ -> typ -> Type.HoleType.t -> Type.HoleType.t
= fun tyvar typ hole_types -> 
  BatMap.map (fun t -> update_type t tyvar typ) hole_types

let rec is_exist : typ -> typ -> bool
= fun t1 t2 ->
  match t1 with
  |TVar _ -> if(t1=t2) then true else false
  |TList t -> is_exist t t2 
  |TTuple tl -> list_fold (fun t r-> r || (is_exist t t2)) tl false
  |TCtor(id,tl) -> list_fold (fun t r-> r || (is_exist t t2)) tl false
  |TArr (ty1,ty2) -> (is_exist ty1 t2) || (is_exist ty2 t2)
  |_ -> false

(******************************************************)
(* Transition relation rules for type-directed search *)
(******************************************************)

let update_sets : int -> typ -> Type.TEnv.t -> type_env -> type_env
= fun hole typ env (hole_types, variable_types) ->
  let hole_types = BatMap.add hole typ hole_types in
  let variable_types = BatMap.add hole env variable_types in
  (hole_types,variable_types)

let determined_type : lexp -> (int * typ) list -> Type.TEnv.t -> type_env -> state option
= fun exp typlst env (hole_types, variable_types) ->
  let (hole_types,variable_types) = list_fold(fun (n,t) (hole_types,variable_types)->
    update_sets n t env (hole_types,variable_types)
  ) typlst (hole_types,variable_types) in
  Some (exp,hole_types,variable_types)

let polymorphic_type : lexp -> (typ * typ) -> (int * typ) list -> Type.TEnv.t -> type_env -> state option
= fun exp (t1,t2) typlst env (hole_types, variable_types) ->
  let hole_types = update_hole_type t1 t2 hole_types in
  let env = update_env t1 t2 env in
  determined_type exp typlst env (hole_types, variable_types)

let rec type_directed : lexp -> typ -> Type.TEnv.t -> type_env -> state option
= fun exp hole_typ env (h_t,h_e) ->
  match snd exp with
  | Const n -> 
    begin match hole_typ with
    |TInt -> Some (exp,h_t,h_e) 
    |TVar _ -> polymorphic_type exp (hole_typ,TInt) [] env (h_t, h_e)
    |_ -> None
    end
  | TRUE | FALSE -> 
    begin match hole_typ with
    |TBool -> Some (exp,h_t,h_e)
    |TVar _ -> polymorphic_type exp (hole_typ,TBool) [] env (h_t, h_e)
    |_ -> None
    end
  | String id ->
    begin match hole_typ with
    |TString -> Some (exp,h_t,h_e)
    |TVar _ -> polymorphic_type exp (hole_typ,TString) [] env (h_t, h_e)
    |_ -> None
    end
  | ADD (e1,e2) | SUB (e1,e2) | MUL (e1,e2) | DIV (e1,e2) | MOD (e1,e2) -> 
    let (n1,n2) = (extract_holenum e1,extract_holenum e2) in 
    let holes_typ = [(n1,TInt);(n2,TInt)] in
    begin match hole_typ with
    |TInt -> determined_type exp holes_typ env (h_t, h_e)
    |TVar _ -> polymorphic_type exp (hole_typ,TInt) holes_typ env (h_t, h_e)
    |_ -> None
    end
  | MINUS e1 -> 
    let n1 = extract_holenum e1 in 
    let holes_typ = [(n1,TInt)] in
    begin match hole_typ with
    |TInt -> determined_type exp holes_typ env (h_t, h_e)
    |TVar _ -> polymorphic_type exp (hole_typ,TInt) holes_typ env (h_t, h_e)
    |_ -> None
    end
  | OR (e1,e2) | AND (e1,e2) -> 
    let (n1,n2) = (extract_holenum e1,extract_holenum e2) in
    let holes_typ = [(n1,TBool);(n2,TBool)] in
    begin match hole_typ with
    |TBool -> determined_type exp holes_typ env (h_t, h_e)
    |TVar _ -> polymorphic_type exp (hole_typ,TBool) holes_typ env (h_t, h_e)
    |_ -> None
    end
  | EQUAL (e1,e2) | NOTEQ (e1,e2) -> 
    let (n1,n2) = (extract_holenum e1,extract_holenum e2) in
    let tv = fresh_tvar () in
    let holes_typ = [(n1,tv);(n2,tv)] in
    begin match hole_typ with
    |TBool -> determined_type exp holes_typ env (h_t, h_e)
    |TVar _ -> polymorphic_type exp (hole_typ,TBool) holes_typ env (h_t, h_e) 
    |_ -> None
    end
  | LESS (e1,e2) | LARGER (e1,e2) | LESSEQ (e1,e2) | LARGEREQ (e1,e2) -> 
    let (n1,n2) = (extract_holenum e1,extract_holenum e2) in
    let holes_typ = [(n1,TInt);(n2,TInt)] in
    begin match hole_typ with
    |TBool -> determined_type exp holes_typ env (h_t, h_e)
    |TVar _ -> polymorphic_type exp (hole_typ,TBool) holes_typ env (h_t, h_e)
    |_ -> None
    end
  | NOT e1 ->  
    let n1 = extract_holenum e1 in
    let holes_typ = [(n1,TBool)] in
    begin match hole_typ with
    |TBool -> determined_type exp holes_typ env (h_t, h_e)
    |TVar _ -> polymorphic_type exp (hole_typ,TBool) holes_typ env (h_t, h_e) 
    |_ -> None
    end
  | AT (e1,e2) -> 
    let (n1,n2) = (extract_holenum e1,extract_holenum e2) in
    begin match hole_typ with
    |TList _ -> determined_type exp [(n1,hole_typ);(n2,hole_typ)] env (h_t, h_e)
    |TVar _ -> 
      let tv = fresh_tvar () in
      polymorphic_type exp (hole_typ,TList(tv)) [(n1,TList(tv));(n2,TList(tv))] env (h_t, h_e)
    |_ -> None
    end
  | DOUBLECOLON (e1,e2) -> 
    let (n1,n2) = (extract_holenum e1,extract_holenum e2) in
    begin match hole_typ with
    |TList (t) -> determined_type exp [(n1,t);(n2,hole_typ)] env (h_t, h_e)
    |TVar _ -> 
      let tv = fresh_tvar () in
      polymorphic_type exp (hole_typ,TList(tv)) [(n1,tv);(n2,TList(tv))] env (h_t, h_e)
    |_ -> None
    end
  | EList l ->
    begin match hole_typ with
    |TList (t) -> determined_type exp (list_map (fun h -> (extract_holenum h,t)) l) env (h_t, h_e) 
    |TVar _ -> 
      let tv = fresh_tvar() in
      polymorphic_type exp (hole_typ,TList(tv)) (list_map (fun h -> (extract_holenum h,tv)) l) env (h_t,h_e) 
    |_ -> None
    end
  | ETuple l ->
    begin match hole_typ with
    |TTuple (tl) -> 
      if(List.length l != List.length tl) then None
      else determined_type exp (list_map (fun (h,t) -> (extract_holenum h,t)) (list_combine l tl)) env (h_t,h_e)
    |TVar _ -> 
      let typ_lst = list_map (fun _ -> fresh_tvar ()) l in
      polymorphic_type exp (hole_typ,TTuple(typ_lst)) (list_map (fun (h,t) -> (extract_holenum h,t)) (list_combine l typ_lst)) env (h_t,h_e)
    |_ -> None
    end
  | IF (e1,e2,e3) -> 
    let (n1,n2,n3) = (extract_holenum e1,extract_holenum e2,extract_holenum e3) in
    determined_type exp [(n1,TBool);(n2,hole_typ);(n3,hole_typ)] env (h_t, h_e)
  | ELet (f, is_rec, args, t, e1, e2) ->
    let (n1,n2) = (extract_holenum e1,extract_holenum e2) in
    let h_t' = BatMap.add n1 t h_t in
    let h_t' = BatMap.add n2 hole_typ h_t' in
    let h_e' = BatMap.add n1 (Type.bind_args env args) h_e in
    let h_e' = if(is_rec) then BatMap.modify n1 (fun env -> Type.let_binding env f t) h_e' else h_e' in
    let h_e' = BatMap.add n2 (Type.let_binding env f t) h_e' in
    Some (exp,h_t',h_e')
  | ECtor (x,l) ->
    let ctor_typ = BatMap.find x env in
    let (t,tl) = get_ctor_type ctor_typ in
    let holes_typ = list_map (fun (h,t) -> (extract_holenum h,t)) (list_combine l tl) in
    begin match hole_typ with
    |TBase _ -> if(hole_typ = t) then determined_type exp holes_typ env (h_t, h_e) else None
    |TVar _ -> polymorphic_type exp (hole_typ,t) holes_typ env (h_t, h_e)
    |_ -> None
    end
  | EFun (arg, e) ->
    let n = extract_holenum e in
    let t = Type.type_of_arg arg in
    begin match hole_typ with
    |TArr (t1,t2) -> 
      let t = (match t with |TVar _ -> t1 |_ -> t) in
      if(t=t1) then
        let arg = Type.update_arg_type arg t1 in
        let env = Type.bind_arg env arg in
        determined_type exp [(n,t2)] env (h_t, h_e) 
      else None
    |TVar _ -> 
      let tv = fresh_tvar () in
      polymorphic_type exp (hole_typ,TArr(t,tv)) [(n,tv)] env (h_t, h_e)
    |_ -> None
    end
  | EMatch(e, bs) ->
    let (ps, es) = List.split bs in
    (* find type of branch condition *)
    let typ_pat = fresh_tvar () in
    let (tenvs, eqns) = List.fold_left (fun (tenvs, eqns) pat ->
      let (tenv, pat_eqn) = Type.gen_pat_equations env pat typ_pat in
      (tenv::tenvs, pat_eqn@eqns)
    ) ([], []) ps 
    in
    let subst = List.fold_left (fun subst (t1, t2) -> Type.unify subst ((Type.Subst.apply t1 subst), Type.Subst.apply t2 subst)) Type.Subst.empty eqns in
    let typ_pat = Type.Subst.apply typ_pat subst in
    (* keep the branch condition type in hole_type *)
    let n = extract_holenum e in
    let h_t' = BatMap.add n typ_pat h_t in
    let h_e' = BatMap.add n env h_e in
    (* compute type env of each branch *)
    let tenvs = List.fold_left (fun tenvs tenv ->
      let tenv = BatMap.foldi (fun x typ tenv ->
        let ty = Type.Subst.apply typ subst in
        BatMap.add x ty tenv
      ) tenv BatMap.empty in
      tenv::tenvs
    ) [] tenvs 
    in 
    (* bound each variable type info to hole in body *)
    let (h_t', h_e') = List.fold_left2 (fun (h_t, h_e) e tenv ->
      let n = (extract_holenum e) in
      let h_t' = BatMap.add n hole_typ h_t in
      let h_e' = BatMap.add n tenv h_e in
      (h_t', h_e')
    ) (h_t', h_e') es tenvs in
    Some (exp,h_t',h_e')
  | EVar x ->
    let x_t = BatMap.find x env in
    begin try
      let subst = Type.unify Type.Subst.empty (hole_typ,x_t) in
      let (h_t,h_e) = list_fold (fun (id,typ) (h_t,h_e) -> 
        let tyvar = TVar id in
        (update_hole_type tyvar typ h_t, update_type_env tyvar typ h_e)
      ) subst (h_t,h_e) in
      Some (exp,h_t,h_e)
    with
    |_ -> None
    end
  | EApp (e1,e2) -> 
    let (n1,n2) = (extract_holenum e1,extract_holenum e2) in
    let tv = fresh_tvar() in
    determined_type exp [(n1,TArr(tv,hole_typ));(n2,tv)] env (h_t,h_e)
  | _ -> raise (Failure (Print.exp_to_string exp ^ " is included in Components set"))


let rec update_components : lexp -> lexp
= fun (l,exp)->
  let exp = 
    begin match exp with
    | ADD (e1,e2) -> ADD (gen_labeled_hole(),gen_labeled_hole())
    | SUB (e1,e2) -> SUB (gen_labeled_hole(),gen_labeled_hole())
    | MUL (e1,e2) -> MUL (gen_labeled_hole(),gen_labeled_hole())
    | DIV (e1,e2) -> DIV (gen_labeled_hole(),gen_labeled_hole())
    | MOD (e1,e2) -> MOD (gen_labeled_hole(),gen_labeled_hole())
    | OR (e1,e2) -> OR (gen_labeled_hole(),gen_labeled_hole())
    | AND (e1,e2) -> AND (gen_labeled_hole(),gen_labeled_hole())
    | LESS (e1,e2) -> LESS (gen_labeled_hole(),gen_labeled_hole())
    | LARGER (e1,e2) -> LARGER (gen_labeled_hole(),gen_labeled_hole())
    | EQUAL (e1,e2) -> EQUAL (gen_labeled_hole(),gen_labeled_hole())
    | NOTEQ (e1,e2) -> NOTEQ (gen_labeled_hole(),gen_labeled_hole())
    | LESSEQ (e1,e2) -> LESSEQ (gen_labeled_hole(),gen_labeled_hole())
    | LARGEREQ (e1,e2)  -> LARGEREQ (gen_labeled_hole(),gen_labeled_hole())
    | AT (e1,e2) -> AT (gen_labeled_hole(),gen_labeled_hole())
    | DOUBLECOLON (e1,e2) -> DOUBLECOLON (gen_labeled_hole(),gen_labeled_hole())
    | ELet (f,is_rec,xs,t,e1,e2) ->ELet (f,is_rec,xs,t,gen_labeled_hole(),gen_labeled_hole())
    | EApp (e1,e2) -> EApp (gen_labeled_hole(),gen_labeled_hole())
    | MINUS e1 -> MINUS (gen_labeled_hole ())
    | NOT e1 -> NOT (gen_labeled_hole ())
    | IF (e1,e2,e3) -> IF (gen_labeled_hole (),gen_labeled_hole (),gen_labeled_hole ())
    | ECtor (x,l) -> ECtor(x,(list_map (fun _ -> gen_labeled_hole()) l))
    | EList l -> EList (list_map (fun _ -> gen_labeled_hole()) l)
    | ETuple l-> ETuple (list_map (fun _ -> gen_labeled_hole()) l)
    | EFun (a,e) -> EFun (a,gen_labeled_hole())
    | EMatch (e,bl) ->
      let (pl,el) = list_split bl in
      EMatch (gen_labeled_hole(),(list_combine pl (list_map (fun _ -> gen_labeled_hole()) el)))
    | _ -> exp
    end 
  in (gen_label(),exp) 

let rec expholes : lexp -> lexp BatSet.t
= fun (l,exp) ->
  match exp with
  | ADD (e1,e2) | SUB (e1,e2) | MUL (e1,e2) | DIV (e1,e2)  | MOD (e1,e2)  | OR (e1,e2) | AND (e1,e2) | LESS (e1,e2)
  | LARGER (e1,e2) | EQUAL (e1,e2) | NOTEQ (e1,e2) | LESSEQ (e1,e2) | LARGEREQ (e1,e2)  | AT (e1,e2) 
  | DOUBLECOLON (e1,e2) | ELet (_,_,_,_,e1,e2) | EApp (e1,e2) -> 
    let t = expholes e1 in
    if(BatSet.is_empty t) then expholes e2
    else t
  | MINUS e1 | NOT e1 | EFun (_,e1) -> expholes e1
  | IF (e1,e2,e3) ->
    let t = expholes e1 in
    if(BatSet.is_empty t) then
      let t = expholes e2 in
      if(BatSet.is_empty t) then
        expholes e3
      else t
    else t
  | ECtor (_,l) | EList l | ETuple l -> list_fold (fun e r -> if(BatSet.is_empty r) then expholes e else r) l BatSet.empty
  | EMatch (e,bl) -> 
    let t = expholes e in
    if(BatSet.is_empty t) then
      (list_fold (fun (_,e) r -> if(BatSet.is_empty r) then expholes e else r) bl BatSet.empty)
    else t
  | Hole _ -> BatSet.singleton (l,exp)
  |_ -> BatSet.empty


let find_expholes : prog -> lexp BatSet.t
= fun decls -> 
  list_fold(fun decl set ->
    match decl with
    | DLet (x,is_rec,args,typ,exp) -> BatSet.union (expholes exp) set
    | DBlock (is_rec, bindings) ->
      List.fold_left (fun set (f, is_rec, args, typ, e) -> BatSet.union (expholes e) set) set bindings
    | _ -> set
  ) decls BatSet.empty

let is_closed : prog -> bool
= fun decls -> BatSet.is_empty (find_expholes decls)

let rec replace_exp' : lexp -> lexp -> lexp -> lexp
= fun (l,e) h c ->
  let e = 
    match e with
    | ADD (e1,e2) -> ADD(replace_exp' e1 h c,replace_exp' e2 h c)
    | SUB (e1,e2) -> SUB(replace_exp' e1 h c,replace_exp' e2 h c)
    | MUL (e1,e2) -> MUL(replace_exp' e1 h c,replace_exp' e2 h c)
    | DIV (e1,e2) -> DIV(replace_exp' e1 h c,replace_exp' e2 h c)
    | MOD (e1,e2) -> MOD(replace_exp' e1 h c,replace_exp' e2 h c)
    | OR (e1,e2) -> OR(replace_exp' e1 h c,replace_exp' e2 h c)
    | AND (e1,e2) -> AND(replace_exp' e1 h c,replace_exp' e2 h c)
    | LESS (e1,e2) -> LESS(replace_exp' e1 h c,replace_exp' e2 h c)
    | LARGER (e1,e2) -> LARGER(replace_exp' e1 h c,replace_exp' e2 h c)
    | EQUAL (e1,e2) -> EQUAL(replace_exp' e1 h c,replace_exp' e2 h c)
    | NOTEQ (e1,e2) -> NOTEQ(replace_exp' e1 h c,replace_exp' e2 h c)
    | LESSEQ (e1,e2) -> LESSEQ(replace_exp' e1 h c,replace_exp' e2 h c)
    | LARGEREQ (e1,e2) -> LARGEREQ(replace_exp' e1 h c,replace_exp' e2 h c)
    | AT (e1,e2) -> AT(replace_exp' e1 h c,replace_exp' e2 h c)
    | DOUBLECOLON (e1,e2) -> DOUBLECOLON(replace_exp' e1 h c,replace_exp' e2 h c)
    | ELet (f,is_rec,xs,t,e1,e2) -> ELet(f,is_rec,xs,t,(replace_exp' e1 h c),replace_exp' e2 h c) 
    | EApp (e1,e2) -> EApp(replace_exp' e1 h c,replace_exp' e2 h c)
    | NOT e1 -> NOT(replace_exp' e1 h c)
    | MINUS e1 -> MINUS(replace_exp' e1 h c)
    | IF (e1,e2,e3) -> IF(replace_exp' e1 h c,replace_exp' e2 h c,replace_exp' e3 h c)
    | ECtor (x,l) -> ECtor(x,replace_exp_list l h c)
    | EList l -> EList (replace_exp_list l h c)
    | ETuple l -> ETuple (replace_exp_list l h c)
    | EMatch (e,bl) ->
      let (pl,el) = List.split bl in
      EMatch((replace_exp' e h c),List.combine pl (replace_exp_list el h c))
    | EFun (a,e) -> EFun(a,replace_exp' e h c)
    | Hole (n) -> if ((l,e) = h) then (snd c) else Hole(n)
    | _ -> e
   in (l,e)
and replace_exp_list : lexp list -> lexp -> lexp -> lexp list
= fun lst hole candidate -> 
  list_map (fun e -> replace_exp' e hole candidate) lst

let rec replace_exp : prog -> lexp -> lexp -> prog
= fun decls hole candidate ->
  match decls with
  [] -> []
  |hd::tl ->
  (
    match hd with
      | DLet (x,is_rec,args,typ,exp) -> 
        DLet (x,is_rec,args,typ, (replace_exp' exp hole candidate)) :: (replace_exp tl hole candidate)
      | DBlock (is_rec, bindings) ->
        (DBlock(is_rec,(List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, (replace_exp' e hole candidate))) bindings))) :: (replace_exp tl hole candidate)
      | x -> x :: (replace_exp tl hole candidate)
  )

let bound_var_to_comp : Type.TEnv.t -> components -> components
= fun tenv cand ->
  BatMap.foldi(fun v t r ->
    match t with
    | TCtor(t,tl) ->
      begin match tl with
      |[] -> BatSet.add (gen_label(), ECtor(v,[])) r
      |hd::tl -> BatSet.add (gen_label(), ECtor(v,[gen_labeled_hole()])) r
      end
    |_ -> BatSet.add (gen_label(), EVar v) r
  ) tenv cand

let except_alias_vars : MustAlias.Sem.equivSet -> components -> components
= fun alias_info candidates ->
  BatSet.fold (fun (p1,p2) cand_set ->
    match p2 with
    |PVar x -> BatSet.filter (fun (_,exp) -> exp <> EVar x) cand_set  
    |_ -> cand_set
  ) alias_info candidates


let gen_exp_nextstates : components -> (Workset.work * lexp) -> Workset.work BatSet.t
= fun candidates ((rank,prog,h_t,h_e),hole) ->
  let n = extract_holenum hole in
  let hole_type = BatMap.find n h_t in
  let candidates =
    begin match hole_type with
    |TTuple lst -> BatSet.add (gen_label(),ETuple (List.map (fun _ -> gen_labeled_hole()) lst)) candidates
    |_ -> candidates
    end in
  let env = BatMap.find n h_e in
  let candidates = bound_var_to_comp env candidates in
  let alias_info = MustAlias.Sem.run prog in
  let candidates = except_alias_vars (MustAlias.Sem.get_aliasSet alias_info n) candidates in
  let nextstates = BatSet.fold (fun c r-> 
    let result = type_directed c hole_type env (h_t,h_e) in
    match result with
    |Some (e,h_t,h_e) -> BatSet.add (e,h_t,h_e) r
    |None -> r
  ) candidates BatSet.empty in
  (*let nextstates = BatSet.fold (fun c r -> BatSet.add (c,BatMap.empty,BatMap.empty) r) (BatSet.union candidates (Bv.run prog n)) BatSet.empty in*)
  BatSet.map (fun (e,h_t,h_e)->
    (rank,replace_exp prog hole e,BatMap.remove n h_t,BatMap.remove n h_e)
  ) nextstates 

let next_of_exp : components -> (Workset.work * lexp) -> Workset.work BatSet.t
= fun exp_set (ranked_prog,hole) ->
  gen_exp_nextstates exp_set (ranked_prog,hole)
  
let next : Workset.work -> components -> Workset.work BatSet.t
= fun (rank,prog,h_t,h_e) components ->
  let exp_holes = find_expholes prog in
  let next_exp = BatSet.fold (fun exp_hole set -> 
    BatSet.union set (next_of_exp components ((rank,prog,h_t,h_e),exp_hole))
  ) exp_holes BatSet.empty in
  next_exp


let rec is_solution : prog -> examples -> bool
= fun prog examples ->
(
  List.for_all (fun (inputs,output) ->
    let res_var = "__res__" in
    let prog = prog@(!grading_pgm) in
    let prog' = prog @ [(DLet (BindOne res_var,false,[],fresh_tvar(),(appify (gen_label(), EVar !Options.opt_entry_func) inputs)))] in
    try
      let (env, _) = Eval.run prog' in
      let result = lookup_env res_var env in
      Eval.value_equality result output
    with
    |TimeoutError -> (*fprintf (!debug) "%s\n" (Print.program_to_string prog);*) false
    |StackOverflow _ -> Eval.infinite_count:=(!Eval.infinite_count)+1; false
    |Stack_overflow -> Eval.infinite_count:=(!Eval.infinite_count)+1; false
    |e -> 
      (*let msg = Printexc.to_string e in
      fprintf (!debug) "%s\n" (Print.program_to_string prog);
      fprintf (!debug) "%s\n" (msg);*)
      false
  ) examples
)

let start_time = ref 0.0
let iter = ref 0
let count = ref 0

let rec work : Workset.t -> components -> examples -> prog option
= fun workset exp_set examples->
  iter := !iter +1;
  if (Sys.time()) -. (!start_time) > 60.0 then None
  (*
  else if (!iter mod 100 = 0)
    then
      begin
        print_string("Iter : " ^ (string_of_int !iter) ^ " ");
        print_endline((Workset.workset_info workset) ^ (" Total elapsed : " ^ (string_of_float (Sys.time() -. !start_time))));
        work workset exp_set examples
      end
  *)
  else
  match Workset.choose workset with
  | None -> None
  | Some ((rank,prog,h_t,h_e),remaining_workset) ->
    if (Infinite.Static.run prog) then
      work remaining_workset exp_set examples
    else
      (*let _ = fprintf (!debug) "%s\n" (Print.program_to_string prog) in*)
      if is_closed prog then
        let _ = count := !count +1 in
        if is_solution prog examples then Some prog
        else work remaining_workset exp_set examples
      else if Smt_pruning.smt_pruning prog examples then
        let exp_set = BatSet.map update_components exp_set in
        let nextstates = next (rank,prog,h_t,h_e) exp_set in
        let new_workset = BatSet.fold Workset.add nextstates remaining_workset in
        work new_workset exp_set examples
      else work remaining_workset exp_set examples

let hole_synthesize : prog -> Workset.work BatSet.t -> components -> examples -> prog option
= fun pgm pgm_set components examples -> 
  (*Print.print_header "expression component set is below";
  Print.print_exp_set components;*)
  let workset = BatSet.fold (fun t set-> Workset.add t set) pgm_set Workset.empty in
  let _ = start_time := 0.0 in
  let result = work workset components examples in
  result