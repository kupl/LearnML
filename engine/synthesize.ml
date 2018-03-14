open Lang
open Util
open Printf

(*
 ******************************************************
 	Code for Synthesizing the "Hole"
 ******************************************************
*)

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
    if explored (Normalize.normalize pgm) (heap,sset) then (heap,sset)
    else
      (Heap.add (n,pgm,h_t,h_e) heap, BatSet.add (Print.program_to_string (Normalize.normalize pgm)) sset)

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
let debug = ref (open_out "debug.txt")

let extract_holenum e = 
	match e with
	|Hole n -> n
	|_ -> raise (Failure "error during obtain hole number")

let get_ctor_type typ = 
	match typ with
	|TCtor (t,tl) -> (t,tl)
	|_ -> raise (Failure "Constructor type does not included at synthesizing")

let tvar_num = ref 0

(* generate a fresh type variable *)
let fresh_tvar () = (tvar_num := !tvar_num + 1; (TVar ("s" ^ string_of_int !tvar_num)))

let rec update_type typ t1 t2= 
	match typ with
	|TVar id -> if(typ=t1) then t2 else typ
	|TList ty -> TList (update_type ty t1 t2)
	|TTuple lst -> TTuple (update_type_lst lst t1 t2)
	|TCtor (ty,lst) -> TCtor (ty,update_type_lst lst t1 t2)
	|TArr (ty1,ty2) -> TArr(update_type ty1 t1 t2,update_type ty2 t1 t2)
	|_ -> typ

and update_type_lst lst t1 t2=
	list_map (fun t -> update_type t t1 t2) lst 

let update_type_env t1 t2 env =
	BatMap.map (fun tenv -> BatMap.map (fun t -> update_type t t1 t2) tenv ) env

let update_env t1 t2 env =
	BatMap.map (fun t -> update_type t t1 t2) env

let update_hole_type t1 t2 h_t = 
	BatMap.map (fun t -> update_type t t1 t2) h_t

let rec is_exist t1 t2 =
	match t1 with
	|TVar _ -> if(t1=t2) then true else false
	|TList t -> is_exist t t2 
	|TTuple tl -> list_fold (fun t r-> r || (is_exist t t2)) tl false
	|TCtor(id,tl) -> list_fold (fun t r-> r || (is_exist t t2)) tl false
	|TArr (t3,t4) -> (is_exist t3 t2) || (is_exist t4 t2)
	|_ -> false

let update_sets hole typ env h_t h_e =
	let h_t' = BatMap.add hole typ h_t in
	let h_e' = BatMap.add hole env h_e in
	(h_t',h_e')

let determined_type exp typlst env h_t h_e =
	let (h_t',h_e') = list_fold(fun (n,t) (h_t,h_e)->
		update_sets n t env h_t h_e
	) typlst (h_t,h_e) in
	Some (exp,h_t',h_e')

let polymorphic_type exp (t1,t2) typlst env h_t h_e =
	let h_t = update_hole_type t1 t2 h_t in
	let env = update_env t1 t2 env in
	determined_type exp typlst env h_t h_e

let rec type_directed exp hole_typ env (h_t,h_e) =
	match exp with
	| Const n -> 
		begin match hole_typ with
		|TInt -> Some (exp,h_t,h_e) 
		|TVar _ -> polymorphic_type exp (hole_typ,TInt) [] env h_t h_e
		|_ -> None
		end
	| TRUE | FALSE -> 
		begin match hole_typ with
		|TBool -> Some (exp,h_t,h_e)
		|TVar _ -> polymorphic_type exp (hole_typ,TBool) [] env h_t h_e
		|_ -> None
		end
	| String id ->
		begin match hole_typ with
		|TString -> Some (exp,h_t,h_e)
		|TVar _ -> polymorphic_type exp (hole_typ,TString) [] env h_t h_e
		|_ -> None
		end
	| ADD (e1,e2) | SUB (e1,e2) | MUL (e1,e2) | DIV (e1,e2) | MOD (e1,e2) -> 
		let (n1,n2) = (extract_holenum e1,extract_holenum e2) in 
		let holes_typ = [(n1,TInt);(n2,TInt)] in
		begin match hole_typ with
		|TInt -> determined_type exp holes_typ env h_t h_e
		|TVar _ -> polymorphic_type exp (hole_typ,TInt) holes_typ env h_t h_e 
		|_ -> None
		end
	| MINUS e1 -> 
		let n1 =extract_holenum e1 in 
		let holes_typ = [(n1,TInt)] in
		begin match hole_typ with
		|TInt -> determined_type exp holes_typ env h_t h_e
		|TVar _ -> polymorphic_type exp (hole_typ,TInt) holes_typ env h_t h_e 
		|_ -> None
		end
	| OR (e1,e2) | AND (e1,e2) -> 
		let (n1,n2) = (extract_holenum e1,extract_holenum e2) in
		let holes_typ = [(n1,TBool);(n2,TBool)] in
		begin match hole_typ with
		|TBool -> determined_type exp holes_typ env h_t h_e
		|TVar _ -> polymorphic_type exp (hole_typ,TBool) holes_typ env h_t h_e
		|_ -> None
		end
	| EQUAL (e1,e2) | NOTEQ (e1,e2) -> 
		let (n1,n2) = (extract_holenum e1,extract_holenum e2) in
		let tv = fresh_tvar () in
		let holes_typ = [(n1,tv);(n2,tv)] in
		begin match hole_typ with
		|TBool ->	determined_type exp holes_typ env h_t h_e
		|TVar _ -> polymorphic_type exp (hole_typ,TBool) holes_typ env h_t h_e 
		|_ -> None
		end
	| LESS (e1,e2) | LARGER (e1,e2) | LESSEQ (e1,e2) | LARGEREQ (e1,e2) -> 
		let (n1,n2) = (extract_holenum e1,extract_holenum e2) in
		let holes_typ = [(n1,TInt);(n2,TInt)] in
		begin match hole_typ with
		|TBool -> determined_type exp holes_typ env h_t h_e
		|TVar _ -> polymorphic_type exp (hole_typ,TBool) holes_typ env h_t h_e
		|_ -> None
		end
	| NOT e1 ->	let n1 = extract_holenum e1 in
		let holes_typ = [(n1,TBool)] in
		begin match hole_typ with
		|TBool -> determined_type exp holes_typ env h_t h_e
		|TVar _ -> polymorphic_type exp (hole_typ,TBool) holes_typ env h_t h_e 
		|_ -> None
		end
	| AT (e1,e2) -> let (n1,n2) = (extract_holenum e1,extract_holenum e2) in
		begin match hole_typ with
		|TList _ -> determined_type exp [(n1,hole_typ);(n2,hole_typ)] env h_t h_e
		|TVar _ -> let tv = fresh_tvar () in
			polymorphic_type exp (hole_typ,TList(tv)) [(n1,TList(tv));(n2,TList(tv))] env h_t h_e
		|_ -> None
		end
	| DOUBLECOLON (e1,e2) -> let (n1,n2) = (extract_holenum e1,extract_holenum e2) in
		begin match hole_typ with
		|TList (t) -> determined_type exp [(n1,t);(n2,hole_typ)] env h_t h_e
		|TVar _ -> let tv = fresh_tvar () in
			polymorphic_type exp (hole_typ,TList(tv)) [(n1,tv);(n2,TList(tv))] env h_t h_e
		|_ -> None
		end
	| EList l ->
		begin match hole_typ with
		|TList (t) -> determined_type exp (list_map (fun h -> let n = extract_holenum h in (n,t)) l) env h_t h_e 
		|TVar _ -> let tv = fresh_tvar() in
			polymorphic_type exp (hole_typ,TList(tv)) (list_map (fun h -> (extract_holenum h,tv)) l) env h_t h_e 
		|_ -> None
		end
	| ETuple l ->
		begin match hole_typ with
		|TTuple (tl) -> 
			if(List.length l != List.length tl) then None
			else determined_type exp (list_map (fun (h,t) -> (extract_holenum h,t)) (list_combine l tl)) env h_t h_e
		|TVar _ -> let t_l = list_map (fun _ -> fresh_tvar ()) l in
			polymorphic_type exp (hole_typ,TTuple(t_l)) (list_map (fun (h,t) -> (extract_holenum h,t)) (list_combine l t_l)) env h_t h_e
		|_ -> None
		end
	| IF (e1,e2,e3) -> let (n1,n2,n3) = (extract_holenum e1,extract_holenum e2,extract_holenum e3) in
		determined_type exp [(n1,TBool);(n2,hole_typ);(n3,hole_typ)] env h_t h_e
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
		|TBase _ -> if(hole_typ = t) then determined_type exp holes_typ env h_t h_e else None
		|TVar _ -> polymorphic_type exp (hole_typ,t) holes_typ env h_t h_e
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
				determined_type exp [(n,t2)] env h_t h_e 
			else None
		|TVar _ -> 
			let tv = fresh_tvar () in
			polymorphic_type exp (hole_typ,TArr(t,tv)) [(n,tv)] env h_t h_e
		|_ -> None
		end
	| EMatch(e, bs) ->
		let (ps, es) = List.split bs in
		(* find type of branch condition *)
		let typ_pat = fresh_tvar () in
		let (tenvs, eqns) = List.fold_left (fun (tenvs, eqns) pat ->
      	let (tenv, pat_eqn) = Type.gen_pat_equations (env, eqns) pat typ_pat in
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
		) (h_t', h_e') es tenvs
		in
		Some (exp,h_t',h_e')
	| EVar x ->
		let x_t = BatMap.find x env in
		let rec check_correct typ x_t h_t env=
			begin match (typ,x_t) with
			| (TVar x,_) -> 
				begin match x_t with
				|TVar y -> if(x=y) then (true,h_t,env) else (true,update_hole_type typ x_t h_t,update_type_env typ x_t env)
				|TList t1 -> if(is_exist t1 typ) then (false,h_t,env) else (true,update_hole_type typ x_t h_t,update_type_env typ x_t env)
				|TTuple tl -> if(List.for_all (fun t -> not (is_exist t typ)) tl) then (true,update_hole_type typ x_t h_t,update_type_env typ x_t env) else (false,h_t,env)
				|TCtor(id,tl) -> if(List.for_all (fun t -> not (is_exist t typ)) tl) then (true,update_hole_type typ x_t h_t,update_type_env typ x_t env) else (false,h_t,env)
				|TArr (t1,t2) -> if((is_exist t1 typ) || (is_exist t2 typ)) then (false,h_t,env) else (true,update_hole_type typ x_t h_t,update_type_env typ x_t env)
				|_ -> (true,update_hole_type typ x_t h_t,update_type_env typ x_t env)
				end
			| (_,TVar _) -> check_correct x_t typ h_t env
			| (TList t1,TList t2) -> check_correct t1 t2 h_t env
			| (TTuple tl1, TTuple tl2) -> ((try (List.for_all2(fun t1 t2 -> let (b,_,_) = check_correct t1 t2 h_t env in b) tl1 tl2) with _-> false),h_t,env)
			| (TCtor (id1,tl1),TCtor (id2,tl2)) ->
				if(id1=id2) then ((try (List.for_all2(fun t1 t2 -> let (b,_,_) = check_correct t1 t2 h_t env in b) tl1 tl2) with _-> false),h_t,env)
				else (false,h_t,env)
			| (TArr (t1,t2),TArr (t3,t4)) ->
				let (b,h_t,env) = check_correct t1 t3 h_t env in
				if(b=false) then (b,h_t,env)
				else check_correct t2 t4 h_t env
			|_ -> if(typ=x_t) then (true,h_t,env) else (false,h_t,env)
		end in
		let (result,h_t',h_e')=check_correct hole_typ x_t h_t h_e in
		if result then Some (exp,h_t',h_e')
		else None
	| EApp (e1,e2) -> 
		let (n1,n2) = (extract_holenum e1,extract_holenum e2) in
		let tv = fresh_tvar() in
		determined_type exp [(n1,TArr(tv,hole_typ));(n2,tv)] env h_t h_e
	| _ -> raise (Failure (Print.exp_to_string exp ^ " is included in Components set"))


let rec update_components : exp -> exp
= fun exp->
	match exp with
	| ADD (e1,e2) -> ADD (gen_hole(),gen_hole())
	| SUB (e1,e2) -> SUB (gen_hole(),gen_hole())
	| MUL (e1,e2) -> MUL (gen_hole(),gen_hole())
	| DIV (e1,e2) -> DIV (gen_hole(),gen_hole())
	| MOD (e1,e2) -> MOD (gen_hole(),gen_hole())
	| OR (e1,e2) -> OR (gen_hole(),gen_hole())
	| AND (e1,e2) -> AND (gen_hole(),gen_hole())
	| LESS (e1,e2) -> LESS (gen_hole(),gen_hole())
	| LARGER (e1,e2) -> LARGER (gen_hole(),gen_hole())
	| EQUAL (e1,e2) -> EQUAL (gen_hole(),gen_hole())
	| NOTEQ (e1,e2) -> NOTEQ (gen_hole(),gen_hole())
	| LESSEQ (e1,e2) -> LESSEQ (gen_hole(),gen_hole())
	| LARGEREQ (e1,e2)  -> LARGEREQ (gen_hole(),gen_hole())
	| AT (e1,e2) -> AT (gen_hole(),gen_hole())
	| DOUBLECOLON (e1,e2) -> DOUBLECOLON (gen_hole(),gen_hole())
	| ELet (f,is_rec,xs,t,e1,e2) ->ELet (f,is_rec,xs,t,gen_hole(),gen_hole())
	| EApp (e1,e2) -> EApp (gen_hole(),gen_hole())
	| MINUS e1 -> MINUS (gen_hole ())
	| NOT e1 -> NOT (gen_hole ())
	| IF (e1,e2,e3) -> IF (gen_hole (),gen_hole (),gen_hole ())
	| ECtor (x,l) -> ECtor(x,(list_map (fun _ -> gen_hole()) l))
	| EList l -> EList (list_map (fun _ -> gen_hole()) l)
	| ETuple l-> ETuple (list_map (fun _ -> gen_hole()) l)
	| EFun (a,e) -> EFun (a,gen_hole())
	| EMatch (e,bl) ->
		let (pl,el) = list_split bl in
		EMatch (gen_hole(),(list_combine pl (list_map (fun _ -> gen_hole()) el)))
	| _ -> exp 

let rec expholes : exp -> exp BatSet.t
= fun exp ->
	match exp with
	| ADD (e1,e2) | SUB (e1,e2) | MUL (e1,e2) | DIV (e1,e2)	| MOD (e1,e2)	| OR (e1,e2) | AND (e1,e2) | LESS (e1,e2)
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
	| Hole _ -> BatSet.singleton exp
	|_ -> BatSet.empty


let find_expholes : prog -> exp BatSet.t
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

let rec replace_exp' : exp -> exp -> exp -> exp
= fun e h c ->
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
	| Hole (n) -> if (e = h) then c else Hole(n)
	| _ -> e
and replace_exp_list lst hole candidate = 
	list_map (fun e -> replace_exp' e hole candidate) lst

let rec replace_exp : prog -> exp -> exp -> prog
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

let bound_var_to_comp tenv cand =
  BatMap.foldi(fun v t r ->
    match t with
    | TCtor(t,tl) ->
      begin match tl with
      |[] -> BatSet.add (ECtor(v,[])) r
      |hd::tl -> BatSet.add (ECtor(v,[gen_hole()])) r
      end
    |_ -> BatSet.add (EVar v) r
  ) tenv cand

let except_alias_vars alias_info candidates =
	BatSet.diff candidates (BatSet.fold (fun (p1,p2) acc -> 
			match p2 with
			|PVar x -> BatSet.add (EVar x) acc
			|_ -> acc
		)alias_info BatSet.empty
	)

let gen_exp_nextstates : exp BatSet.t -> (Workset.work * exp) -> Workset.work BatSet.t
= fun candidates ((rank,prog,h_t,h_e),hole) ->
	let n = extract_holenum hole in
	let hole_type = BatMap.find n h_t in
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
	BatSet.map (fun (e,h_t,h_e)->
		(rank,replace_exp prog hole e,BatMap.remove n h_t,BatMap.remove n h_e)
	) nextstates 

let next_of_exp : components -> (Workset.work * exp) -> Workset.work BatSet.t
= fun exp_set (ranked_prog,exp_hole) ->
	gen_exp_nextstates exp_set (ranked_prog,exp_hole)
	
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
    let prog = prog@(External.grading_prog) in
		let prog' = prog @ [(DLet (BindOne res_var,false,[],fresh_tvar(),(appify (EVar !Options.opt_entry_func) inputs)))] in
		try
			let env = Eval.run prog' in
			let result = lookup_env res_var env in
      Eval.value_equality result output
		with
    |TimeoutError -> (*fprintf (!debug) "%s\n" (Print.program_to_string prog);*) false
		|StackOverflow _ -> Eval.infinite_count:=(!Eval.infinite_count)+1; false
		|Stack_overflow -> Eval.infinite_count:=(!Eval.infinite_count)+1; false
		|e -> 
			let msg = Printexc.to_string e in
			fprintf (!debug) "%s\n" (Print.program_to_string prog);
			fprintf (!debug) "%s\n" (msg);
			false
	) examples
)

let start_time = ref 0.0
let iter = ref 0
let count = ref 0

let rec work : Workset.t -> components -> examples -> prog option
= fun workset exp_set examples->
	iter := !iter +1;
  if (Sys.time()) -. (!start_time) > 300.0 then None
  else if (!iter mod 10000 = 0)
	  then
		  begin
			  print_string("Iter : " ^ (string_of_int !iter) ^ " ");
			  print_endline((Workset.workset_info workset) ^ (" Total elapsed : " ^ (string_of_float (Sys.time() -. !start_time))));
			  work workset exp_set examples
		  end
	else
	match Workset.choose workset with
	| None -> None
	| Some ((rank,prog,h_t,h_e),remaining_workset) ->
		if (Infinite.Static.run prog) then
			work remaining_workset exp_set examples
  		else if is_closed prog then
		  	let _ = count := !count +1 in
		  	if is_solution prog examples then Some prog
				else work remaining_workset exp_set examples
			else if Smt_pruning.smt_pruning prog examples then
	    	let exp_set = BatSet.map update_components exp_set in
				let nextstates = next (rank,prog,h_t,h_e) exp_set in
				let new_workset = BatSet.fold Workset.add nextstates remaining_workset in
				work new_workset exp_set examples
			else work remaining_workset exp_set examples

let hole_synthesize : prog -> Workset.work BatSet.t -> components -> examples ->prog option
= fun pgm pgm_set components examples -> 
	Print.print_header "expression component set is below";
	Print.print_exp_set components;
	let workset = BatSet.fold (fun t set-> Workset.add t set) pgm_set Workset.empty in
  	let _ = start_time := 0.0 in
	let result = work workset components examples in
	let result_prog_string = 
	match result with
	|None -> "None"
	|Some prog -> Print.program_to_string (prog) in
	Print.print_header "original" ;
	Print.print_pgm pgm;
	Print.print_header "result";
	print_endline(result_prog_string);
	Print.print_header "Total time";
	print_endline(string_of_float (Sys.time() -. !start_time));
	Print.print_header "eval count";
	print_endline(string_of_int (!Eval.count));
	Print.print_header "infinite count";
	print_endline(string_of_int (!Eval.infinite_count + !Symbol_eval.infinite_count));
	Print.print_header "SMT time";
	print_endline(string_of_float (!Smt_pruning.smt_time));
	Print.print_header "UNSAT count";
	print_endline(string_of_int (!Smt_pruning.unsat_count));
	result

