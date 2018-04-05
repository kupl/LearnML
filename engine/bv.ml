open Lang
open Util

let rec arg2var : arg -> exp BatSet.t
= fun arg -> 
	match arg with
	|ArgUnder _-> BatSet.empty
	|ArgOne (x,_) -> BatSet.singleton (EVar x)
	|ArgTuple l -> list_fold (fun arg r -> BatSet.union (arg2var arg) r) l BatSet.empty

let args2var : arg list -> exp BatSet.t
= fun args -> list_fold (fun arg r -> BatSet.union (arg2var arg) r) args BatSet.empty

let rec bind2var : let_bind -> exp BatSet.t
= fun bind ->
	match bind with
	| BindUnder -> BatSet.empty
	| BindOne x -> BatSet.singleton (EVar x)
	| BindTuple l -> list_fold (fun bind r -> BatSet.union (bind2var bind) r) l BatSet.empty

let rec pat2var : pat -> exp BatSet.t
= fun p ->
	match p with
	| PUnit | PInt _ | PBool _ | PUnder -> BatSet.empty
	| PVar x -> BatSet.singleton (EVar x)
	| PTuple l | PList l | PCons l | PCtor (_,l) | Pats l -> list_fold (fun p r -> BatSet.union r (pat2var p)) l BatSet.empty

let rec bv_exp : exp -> exp BatSet.t -> (int,exp BatSet.t) BatMap.t -> (int,exp BatSet.t) BatMap.t
= fun exp env table->
	match exp with
	| EUnit | Const _ | TRUE | FALSE | EVar _ | String _ -> table
	| MINUS e | NOT e | Raise e -> bv_exp e env table
	| ADD (e1,e2) | SUB (e1,e2) | MUL (e1,e2) | DIV (e1,e2) | MOD (e1,e2) | OR (e1,e2)
	| AND (e1,e2) | LESS (e1,e2) | LARGER (e1,e2) | EQUAL (e1,e2) | NOTEQ (e1,e2) | LESSEQ (e1,e2)
	| LARGEREQ (e1,e2) | AT (e1,e2) | DOUBLECOLON (e1,e2) | STRCON (e1,e2) | EApp (e1,e2) -> 
		bv_exp e1 env (bv_exp e2 env table)
	| IF (e1,e2,e3) -> bv_exp e1 env (bv_exp e2 env (bv_exp e3 env table))
	| EList l | ETuple l | ECtor (_,l) -> list_fold (fun e r -> bv_exp e env r) l table
	| EFun (arg,e) -> bv_exp e (BatSet.union env (arg2var arg)) table
	| ELet (f,is_rec,args,t,e1,e2) ->
		let e2_env = BatSet.union env (bind2var f) in
		let e1_env = BatSet.union (BatSet.union env (args2var args)) (if is_rec then bind2var f else BatSet.empty) in
		bv_exp e1 e1_env (bv_exp e2 e2_env table)
	| EBlock (is_rec,bindings,e) ->
		let function_names = list_fold (fun (bind,_,_,_,_) r -> BatSet.union r (bind2var bind)) bindings BatSet.empty in
		if is_rec then
			let table = (list_fold (fun (_,_,args,_,e) r -> bv_exp e (BatSet.union (BatSet.union env function_names) (args2var args)) r) bindings table)in
			 (bv_exp e (BatSet.union env function_names) table)
		else
			let table = (list_fold (fun (_,_,args,_,e) r -> bv_exp e (BatSet.union env (args2var args)) r) bindings table) in
				bv_exp e (BatSet.union env function_names) table
	| EMatch (e,bs) ->
		let table = (list_fold (fun (p,e) r -> bv_exp e (BatSet.union env (pat2var p)) r) bs table) in
		bv_exp e env table
	| Hole n -> BatMap.add n env table

let tctor2exp : ctor -> exp
=fun (x,l) ->
	match l with
	|[] -> ECtor (x,[])
	|hd::tl -> ECtor (x,[Hole 0])

let rec bv_tdecls : prog -> exp BatSet.t -> exp BatSet.t
= fun decls env ->
	match decls with
	|[] -> env
	|hd::tl ->
		begin match hd with
		|DData(t,ctors) -> bv_tdecls tl (list_fold (fun tctor r -> BatSet.add (tctor2exp tctor) r) ctors env)
		|_ -> env
		end

let rec bv_decls : prog -> exp BatSet.t -> (int,exp BatSet.t) BatMap.t -> (int,exp BatSet.t) BatMap.t
= fun decls env table ->
	match decls with
	|[] -> table
	|hd::tl ->
		begin match hd with
		| DLet (f, is_rec, args, t, e) ->
			if is_rec then
				let table = bv_exp e (BatSet.union (BatSet.union (bind2var f) (args2var args)) env) table in
				bv_decls tl (BatSet.union env (bind2var f)) table
			else 
				let table = bv_exp e (BatSet.union (bind2var f) env) table in
				bv_decls tl (BatSet.union env (bind2var f)) table
		| DBlock (is_rec,bindings) ->
			let function_names = list_fold (fun (bind,_,_,_,_) r -> BatSet.union r (bind2var bind)) bindings BatSet.empty in

			if is_rec then
				let table = list_fold (fun (_,_,args,_,e) r -> bv_exp e (BatSet.union env (BatSet.union (args2var args) function_names)) r) bindings table in
				bv_decls tl (BatSet.union env function_names) table
			else
				let table = list_fold (fun (_,_,args,_,e) r -> bv_exp e (BatSet.union env (args2var args)) r) bindings table in
				bv_decls tl (BatSet.union env function_names) table
		| DData (t,ctors)-> bv_decls tl (list_fold (fun tctor r -> BatSet.add (tctor2exp tctor) r) ctors env) table
		| TBlock tdecls -> bv_decls tl (bv_tdecls tdecls env) table
		| DExcept ctor -> bv_decls tl (BatSet.union env (BatSet.singleton (tctor2exp ctor))) table
		|_ -> bv_decls tl env table
		end

let run : prog -> int -> exp BatSet.t
= fun pgm hole ->
	let table = bv_decls pgm BatSet.empty BatMap.empty in
	BatMap.find hole table
