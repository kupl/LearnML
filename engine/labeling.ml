(* labeling program *)
open Lang
open Util
open Symbol_lang

let label_count = ref 0
let new_label() = (label_count := !label_count+1; !label_count)

let rec labeling_pat : pat -> labeled_pat
= fun p ->
	let _ = label_count:= !label_count+1 in
	let label = !label_count in
	match p with
	|PCtor (x,pl) -> (label,PCtor(x,labeling_patlist pl))
	|Pats pl -> (label,Pats (labeling_patlist pl))
	|PInt n -> (label,PInt (n))
	|PVar x -> (label,PVar (x))
	|PBool b -> (label,PBool (b))
	|PList pl -> (label,PList (labeling_patlist pl))
	|PTuple pl -> (label,PTuple (labeling_patlist pl))
	|PUnder -> (label,PUnder)
	|PCons pl -> (label,PCons (labeling_patlist pl))
	|PHole n -> (label, PHole n)

and labeling_patlist : pat list -> labeled_pat list
= fun pl ->
	match pl with
	|[] -> []
	|hd::tl -> 
		let labeled_p = labeling_pat hd in
		(labeled_p)::(labeling_patlist tl)

let rec labeling_exp : exp -> labeled_exp
= fun exp ->
	let label = new_label() in
	match exp with
	|Const n -> (label,Const (n))
	|TRUE -> (label,TRUE)
	|FALSE -> (label,FALSE)
	|EVar x -> (label,EVar (x))
	|ADD (e1,e2) -> 
		let labeled_e1 = labeling_exp e1 in
		let labeled_e2 = labeling_exp e2 in
		(label,ADD (labeled_e1,labeled_e2))
	|SUB (e1,e2) -> 
		let labeled_e1 = labeling_exp e1 in
		let labeled_e2 = labeling_exp e2 in
		(label,SUB (labeled_e1,labeled_e2))
	|MUL (e1,e2) -> 
		let labeled_e1 = labeling_exp e1 in
		let labeled_e2 = labeling_exp e2 in
		(label,MUL (labeled_e1,labeled_e2))
	|DIV (e1,e2) -> 
		let labeled_e1 = labeling_exp e1 in
		let labeled_e2 = labeling_exp e2 in
		(label,DIV (labeled_e1,labeled_e2))
	|MOD (e1,e2) -> 
		let labeled_e1 = labeling_exp e1 in
		let labeled_e2 = labeling_exp e2 in
		(label,MOD (labeled_e1,labeled_e2))
	|OR (e1,e2) -> 
		let labeled_e1 = labeling_exp e1 in
		let labeled_e2 = labeling_exp e2 in
		(label,OR (labeled_e1,labeled_e2))
	|AND (e1,e2) -> 
		let labeled_e1 = labeling_exp e1 in
		let labeled_e2 = labeling_exp e2 in
		(label,AND (labeled_e1,labeled_e2))
	|LESS (e1,e2) -> 
		let labeled_e1 = labeling_exp e1 in
		let labeled_e2 = labeling_exp e2 in
		(label,LESS (labeled_e1,labeled_e2))
	|LARGER (e1,e2) -> 
		let labeled_e1 = labeling_exp e1 in
		let labeled_e2 = labeling_exp e2 in
		(label,LARGER (labeled_e1,labeled_e2))
	|EQUAL (e1,e2) -> 
		let labeled_e1 = labeling_exp e1 in
		let labeled_e2 = labeling_exp e2 in
		(label,EQUAL (labeled_e1,labeled_e2))
	|NOTEQ (e1,e2) -> 
		let labeled_e1 = labeling_exp e1 in
		let labeled_e2 = labeling_exp e2 in
		(label,NOTEQ (labeled_e1,labeled_e2))
	|LESSEQ (e1,e2) -> 
		let labeled_e1 = labeling_exp e1 in
		let labeled_e2 = labeling_exp e2 in
		(label,LESSEQ (labeled_e1,labeled_e2))
	|LARGEREQ (e1,e2) -> 
		let labeled_e1 = labeling_exp e1 in
		let labeled_e2 = labeling_exp e2 in
		(label,LARGEREQ (labeled_e1,labeled_e2))
	|EApp (e1,e2) -> 
		let labeled_e1 = labeling_exp e1 in
		let labeled_e2 = labeling_exp e2 in
		(label,EApp (labeled_e1,labeled_e2))
	|AT (e1,e2) -> 
		let labeled_e1 = labeling_exp e1 in
		let labeled_e2 = labeling_exp e2 in
		(label,AT (labeled_e1,labeled_e2))
	|DOUBLECOLON (e1,e2) -> 
		let labeled_e1 = labeling_exp e1 in
		let labeled_e2 = labeling_exp e2 in
		(label,DOUBLECOLON (labeled_e1,labeled_e2))
	|MINUS e1 -> 
		let labeled_e1 = labeling_exp e1 in
		(label,MINUS (labeled_e1))
	|NOT e1 -> 
		let labeled_e1 = labeling_exp e1 in
		(label,NOT (labeled_e1))
	|EFun (a,e) -> 
		let labeled_e1 = labeling_exp e in
		(label,EFun (a,labeled_e1))
	|ELet (x,is_rec,args,t,e1,e2) -> 
		let labeled_e1 = labeling_exp e1 in
		let labeled_e2 = labeling_exp e2 in
		(label,ELet (x,is_rec,args,t,labeled_e1,labeled_e2))
	|ECtor (x,exps) -> (label,ECtor (x,labeling_explist exps))
	|EMatch (e,bl) -> 
		let labeled_e = labeling_exp e in
		(label,EMatch (labeled_e,labeling_blist bl))
	|IF (e1,e2,e3) -> 
		let labeled_e1 = labeling_exp e1 in
		let labeled_e2 = labeling_exp e2 in
		let labeled_e3 = labeling_exp e3 in
		(label,IF (labeled_e1,labeled_e2,labeled_e3))
	|EList (exps) -> (label,EList (labeling_explist exps))
	|ETuple (exps) -> (label,ETuple (labeling_explist exps))
	|Hole n -> (label, Hole n)

and labeling_explist : exp list -> labeled_exp list
= fun e_l ->
	match e_l with
	|[] -> []
	|hd::tl -> 
		let labeled_e = labeling_exp hd in
			labeled_e :: (labeling_explist tl)

and labeling_blist : branch list -> l_bl list
= fun bl ->
	match bl with
	| [] -> []
	| (p,e)::tl -> 
		let labeled_p = labeling_pat p in
		let labeled_e = labeling_exp e in
		(labeled_p,labeled_e) :: (labeling_blist tl)

let labeling_decl : decl -> labeled_decl
= fun decl ->
	match decl with
	| DData (id,l) -> DData(id, l)
	| DLet (id,is_rec,args,t,e) ->
		let labeled_e = labeling_exp e in
		DLet (id, is_rec, args, t, labeled_e)

let rec labeling_decls : decl list -> labeled_prog -> labeled_prog
= fun decls labeled ->
	match decls with
	| [] -> labeled
	| hd::tl -> 
		let l_decl = labeling_decl hd in
		labeling_decls tl (labeled@[l_decl])

let labeling_prog : prog -> labeled_prog
= fun pgm -> 
	label_count := 0;
	labeling_decls pgm []

(**************)
(* unlabeling *)
(**************)

let rec unlabeling_pat : labeled_pat -> pat
= fun (l, p) ->
	match p with
	|PCtor (x,pl) -> PCtor(x, unlabeling_patlist pl)
	|Pats pl -> Pats (unlabeling_patlist pl)
	|PInt n -> PInt (n)
	|PVar x -> PVar (x)
	|PBool b -> PBool (b)
	|PList pl -> PList (unlabeling_patlist pl)
	|PTuple pl -> PTuple (unlabeling_patlist pl)
	|PUnder -> PUnder
	|PCons pl -> PCons (unlabeling_patlist pl)
	|PHole n -> PHole n

and unlabeling_patlist : labeled_pat list -> pat list
= fun pl ->
	match pl with
	|[] -> []
	|hd::tl -> 
		let unlabeled_p = unlabeling_pat hd in
		(unlabeled_p)::(unlabeling_patlist tl)

let rec unlabeling_exp : labeled_exp -> exp
= fun (l, exp) ->
	match exp with
	|Const n -> Const n
	|TRUE -> TRUE
	|FALSE -> FALSE
	|EVar x -> EVar x
	|ADD (e1,e2) -> 
		let unlabeled_e1 = unlabeling_exp e1 in
		let unlabeled_e2 = unlabeling_exp e2 in
		ADD (unlabeled_e1, unlabeled_e2)
	|SUB (e1,e2) -> 
		let unlabeled_e1 = unlabeling_exp e1 in
		let unlabeled_e2 = unlabeling_exp e2 in
		SUB (unlabeled_e1, unlabeled_e2)
	|MUL (e1,e2) -> 
		let unlabeled_e1 = unlabeling_exp e1 in
		let unlabeled_e2 = unlabeling_exp e2 in
		MUL (unlabeled_e1, unlabeled_e2)
	|DIV (e1,e2) -> 
		let unlabeled_e1 = unlabeling_exp e1 in
		let unlabeled_e2 = unlabeling_exp e2 in
		DIV (unlabeled_e1, unlabeled_e2)
	|MOD (e1,e2) -> 
		let unlabeled_e1 = unlabeling_exp e1 in
		let unlabeled_e2 = unlabeling_exp e2 in
		MOD (unlabeled_e1, unlabeled_e2)
	|OR (e1,e2) -> 
		let unlabeled_e1 = unlabeling_exp e1 in
		let unlabeled_e2 = unlabeling_exp e2 in
		OR (unlabeled_e1, unlabeled_e2)
	|AND (e1,e2) -> 
		let unlabeled_e1 = unlabeling_exp e1 in
		let unlabeled_e2 = unlabeling_exp e2 in
		AND (unlabeled_e1, unlabeled_e2)
	|LESS (e1,e2) -> 
		let unlabeled_e1 = unlabeling_exp e1 in
		let unlabeled_e2 = unlabeling_exp e2 in
		LESS (unlabeled_e1, unlabeled_e2)
	|LARGER (e1,e2) -> 
		let unlabeled_e1 = unlabeling_exp e1 in
		let unlabeled_e2 = unlabeling_exp e2 in
		LARGER (unlabeled_e1, unlabeled_e2)
	|EQUAL (e1,e2) -> 
		let unlabeled_e1 = unlabeling_exp e1 in
		let unlabeled_e2 = unlabeling_exp e2 in
		EQUAL (unlabeled_e1, unlabeled_e2)
	|NOTEQ (e1,e2) -> 
		let unlabeled_e1 = unlabeling_exp e1 in
		let unlabeled_e2 = unlabeling_exp e2 in
		NOTEQ (unlabeled_e1, unlabeled_e2)
	|LESSEQ (e1,e2) -> 
		let unlabeled_e1 = unlabeling_exp e1 in
		let unlabeled_e2 = unlabeling_exp e2 in
		LESSEQ (unlabeled_e1, unlabeled_e2)
	|LARGEREQ (e1,e2) -> 
		let unlabeled_e1 = unlabeling_exp e1 in
		let unlabeled_e2 = unlabeling_exp e2 in
		LARGEREQ (unlabeled_e1, unlabeled_e2)
	|EApp (e1,e2) -> 
		let unlabeled_e1 = unlabeling_exp e1 in
		let unlabeled_e2 = unlabeling_exp e2 in
		EApp (unlabeled_e1, unlabeled_e2)
	|AT (e1,e2) -> 
		let unlabeled_e1 = unlabeling_exp e1 in
		let unlabeled_e2 = unlabeling_exp e2 in
		AT (unlabeled_e1, unlabeled_e2)
	|DOUBLECOLON (e1,e2) -> 
		let unlabeled_e1 = unlabeling_exp e1 in
		let unlabeled_e2 = unlabeling_exp e2 in
		DOUBLECOLON (unlabeled_e1, unlabeled_e2)
	|MINUS e1 -> 
		let unlabeled_e1 = unlabeling_exp e1 in
		MINUS (unlabeled_e1)
	|NOT e1 -> 
		let unlabeled_e1 = unlabeling_exp e1 in
		NOT (unlabeled_e1)
	|EFun (a,e) -> 
		let unlabeled_e1 = unlabeling_exp e in
		EFun (a, unlabeled_e1)
	|ELet (x,is_rec,args,t,e1,e2) -> 
		let unlabeled_e1 = unlabeling_exp e1 in
		let unlabeled_e2 = unlabeling_exp e2 in
		ELet (x,is_rec,args,t,unlabeled_e1,unlabeled_e2)
	|ECtor (x,exps) -> ECtor (x,unlabeling_explist exps)
	|EMatch (e,bl) -> 
		let unlabeled_e = unlabeling_exp e in
		EMatch (unlabeled_e,unlabeling_blist bl)
	|IF (e1,e2,e3) -> 
		let unlabeled_e1 = unlabeling_exp e1 in
		let unlabeled_e2 = unlabeling_exp e2 in
		let unlabeled_e3 = unlabeling_exp e3 in
		IF (unlabeled_e1, unlabeled_e2, unlabeled_e3)
	|EList (exps) -> EList (unlabeling_explist exps)
	|ETuple (exps) -> ETuple (unlabeling_explist exps)
	|Hole n -> Hole n

and unlabeling_explist : labeled_exp list -> exp list
= fun e_l ->
	match e_l with
	|[] -> []
	|hd::tl -> 
		let unlabeled_e = unlabeling_exp hd in
			unlabeled_e :: (unlabeling_explist tl)

and unlabeling_blist : l_bl list -> branch list
= fun bl ->
	match bl with
	| [] -> []
	| (p,e)::tl -> 
		let unlabeled_p = unlabeling_pat p in
		let unlabeled_e = unlabeling_exp e in
		(unlabeled_p, unlabeled_e) :: (unlabeling_blist tl)

let unlabeling_decl : labeled_decl -> decl
= fun l_decl ->
	match l_decl with
	| DData (id,l) -> DData (id,l)
	| DLet (id,is_rec,args,t,e) ->
		let unlabeled_e = unlabeling_exp e in
		DLet (id, is_rec, args, t, unlabeled_e)

let rec unlabeling_decls : labeled_prog -> decl list -> decl list
= fun l_decls decls->
	match l_decls with
	| [] -> decls
	| hd::tl -> 
		let decl = unlabeling_decl hd in
		unlabeling_decls tl (decls@[decl])

let unlabeling_prog : labeled_prog -> prog
= fun l_pgm -> unlabeling_decls l_pgm []

let rec unlabeling_value : labeled_value -> value
= fun lv ->
	match lv with
	| VInt n -> VInt n
	| VBool b -> VBool b
	| VList vs -> VList (List.map unlabeling_value vs) 
	| VTuple vs -> VTuple (List.map unlabeling_value vs) 
	| VCtor (x, vs) -> VCtor (x, List.map unlabeling_value vs) 
	| VFun (x, exp, lenv, senv) -> VFun (x, unlabeling_exp exp, unlabeling_env lenv)
	| VFunRec (f, x, exp, lenv, senv) -> VFunRec (f, x, unlabeling_exp exp, unlabeling_env lenv)
	(*
	| VPFun (of (labeled_value * labeled_value)) list
	*)
	| VHole n -> VHole n
	
and unlabeling_env : labeled_env -> env
= fun lenv ->
	BatMap.map (
		fun v -> unlabeling_value v
	) lenv
(***********************)
(*** generate hole *****)
(***********************)

let labeled_hole_count = ref 0
let gen_labeled_hole : unit -> lexp
= fun () -> 
	labeled_hole_count:=!labeled_hole_count+1;
	exp_hole_count:=!exp_hole_count+1;
	Hole(!labeled_hole_count)

let labeled_pat_hole_count = ref 0
let gen_labeled_pat_hole : unit -> lpat
= fun () -> 
	labeled_pat_hole_count:=!labeled_pat_hole_count+1;
	pat_hole_count:=!pat_hole_count+1; 
	PHole(!labeled_pat_hole_count)

let rec gen_hole_pat : labeled_pat -> int -> labeled_pat
= fun (label, p) n ->
	if label = n then (label, gen_labeled_pat_hole()) else
	match p with
	|PCtor (x,pl) -> (label,PCtor(x, gen_hole_patlist pl n))
	|Pats pl -> (label,Pats (gen_hole_patlist pl n))
	|PInt n -> (label,PInt (n))
	|PVar x -> (label,PVar (x))
	|PBool b -> (label,PBool (b))
	|PList pl -> (label,PList (gen_hole_patlist pl n))
	|PTuple pl -> (label,PTuple (gen_hole_patlist pl n))
	|PUnder -> (label,PUnder)
	|PCons pl -> (label,PCons (gen_hole_patlist pl n))
	|PHole n -> (label, PHole n)

and gen_hole_patlist : labeled_pat list -> int -> labeled_pat list
= fun pl n ->
	match pl with
	|[] -> []
	|hd::tl ->  
		let partial_p = gen_hole_pat hd n in
		(partial_p)::(gen_hole_patlist tl n)

let rec gen_hole_exp : labeled_exp -> int -> labeled_exp
= fun (label, exp) n ->
	if label = n then (label, gen_labeled_hole()) else 
	match exp with 
	|Const n -> (label, Const (n))
	|TRUE -> (label, TRUE)
	|FALSE -> (label, FALSE)
	|EVar x -> (label, EVar (x))
	|ADD (e1,e2) -> 
		let partial_e1 = gen_hole_exp e1 n in
		let partial_e2 = gen_hole_exp e2 n in
		(label, ADD (partial_e1, partial_e2))
	|SUB (e1,e2) -> 
		let partial_e1 = gen_hole_exp e1 n in
		let partial_e2 = gen_hole_exp e2 n in
		(label,SUB (partial_e1,partial_e2))
	|MUL (e1,e2) -> 
		let partial_e1 = gen_hole_exp e1 n in
		let partial_e2 = gen_hole_exp e2 n in
		(label,MUL (partial_e1,partial_e2))
	|DIV (e1,e2) -> 
		let partial_e1 = gen_hole_exp e1 n in
		let partial_e2 = gen_hole_exp e2 n in
		(label,DIV (partial_e1,partial_e2))
	|MOD (e1,e2) -> 
		let partial_e1 = gen_hole_exp e1 n in
		let partial_e2 = gen_hole_exp e2 n in
		(label,MOD (partial_e1,partial_e2))
	|OR (e1,e2) -> 
		let partial_e1 = gen_hole_exp e1 n in
		let partial_e2 = gen_hole_exp e2 n in
		(label,OR (partial_e1,partial_e2))
	|AND (e1,e2) -> 
		let partial_e1 = gen_hole_exp e1 n in
		let partial_e2 = gen_hole_exp e2 n in
		(label,AND (partial_e1,partial_e2))
	|LESS (e1,e2) -> 
		let partial_e1 = gen_hole_exp e1 n in
		let partial_e2 = gen_hole_exp e2 n in
		(label,LESS (partial_e1,partial_e2))
	|LARGER (e1,e2) -> 
		let partial_e1 = gen_hole_exp e1 n in
		let partial_e2 = gen_hole_exp e2 n in
		(label,LARGER (partial_e1,partial_e2))
	|EQUAL (e1,e2) -> 
		let partial_e1 = gen_hole_exp e1 n in
		let partial_e2 = gen_hole_exp e2 n in
		(label,EQUAL (partial_e1,partial_e2))
	|NOTEQ (e1,e2) -> 
		let partial_e1 = gen_hole_exp e1 n in
		let partial_e2 = gen_hole_exp e2 n in
		(label,NOTEQ (partial_e1,partial_e2))
	|LESSEQ (e1,e2) -> 
		let partial_e1 = gen_hole_exp e1 n in
		let partial_e2 = gen_hole_exp e2 n in
		(label,LESSEQ (partial_e1,partial_e2))
	|LARGEREQ (e1,e2) -> 
		let partial_e1 = gen_hole_exp e1 n in
		let partial_e2 = gen_hole_exp e2 n in
		(label,LARGEREQ (partial_e1,partial_e2))
	|EApp (e1,e2) -> 
		let partial_e1 = gen_hole_exp e1 n in
		let partial_e2 = gen_hole_exp e2 n in
		(label,EApp (partial_e1,partial_e2))
	|AT (e1,e2) -> 
		let partial_e1 = gen_hole_exp e1 n in
		let partial_e2 = gen_hole_exp e2 n in
		(label,AT (partial_e1,partial_e2))
	|DOUBLECOLON (e1,e2) -> 
		let partial_e1 = gen_hole_exp e1 n in
		let partial_e2 = gen_hole_exp e2 n in
		(label,DOUBLECOLON (partial_e1,partial_e2))
	|MINUS e1 -> 
		let partial_e1 = gen_hole_exp e1 n in
		(label,MINUS (partial_e1))
	|NOT e1 -> 
		let partial_e1 = gen_hole_exp e1 n in
		(label,NOT (partial_e1))
	|EFun (a,e) -> 
		let partial_e1 = gen_hole_exp e n in
		(label,EFun (a,partial_e1))
	|ELet (x,is_rec,args,t,e1,e2) -> 
		let partial_e1 = gen_hole_exp e1 n in
		let partial_e2 = gen_hole_exp e2 n in
		(label,ELet (x,is_rec,args,t,partial_e1,partial_e2))
	|ECtor (x,exps) -> (label,ECtor (x,gen_hole_explist exps n))
	|EMatch (e,bl) -> 
		let partial_e = gen_hole_exp e n in
		(label,EMatch (partial_e, gen_hole_blist bl n))
	|IF (e1,e2,e3) -> 
		let partial_e1 = gen_hole_exp e1 n in
		let partial_e2 = gen_hole_exp e2 n in
		let partial_e3 = gen_hole_exp e3 n in
		(label,IF (partial_e1,partial_e2,partial_e3))
	|EList (exps) -> (label,EList (gen_hole_explist exps n))
	|ETuple (exps) -> (label,ETuple (gen_hole_explist exps n))
	|Hole n -> (label, Hole n)

and gen_hole_explist : labeled_exp list -> int -> labeled_exp list
= fun e_l n ->
	match e_l with
	|[] -> []
	|hd::tl -> 
		let partial_e = gen_hole_exp hd n in
			partial_e :: (gen_hole_explist tl n)

and gen_hole_blist : l_bl list -> int -> l_bl list
= fun bl n ->
	match bl with
	| [] -> []
	| (p,e)::tl -> 
		let partial_p = gen_hole_pat p n in
		let partial_e = gen_hole_exp e n in
		(partial_p, partial_e) :: (gen_hole_blist tl n)

let rec gen_hole_decl : labeled_decl -> int -> labeled_decl
= fun decl n ->
	match decl with
	| DData (id,l) -> DData (id,l)
	| DLet (id,is_rec,args,t,e) ->
		let partial_exp = gen_hole_exp e n in
		DLet (id, is_rec, args, t, partial_exp)


let rec gen_hole_decls : labeled_prog -> int -> labeled_decl list  -> labeled_prog
= fun pgm n decls->
  match pgm with
  | [] -> decls
  | hd::tl -> 
  	let decl = gen_hole_decl hd n in
  	gen_hole_decls tl n (decls@[decl])


let rec gen_hole_pgm : labeled_prog -> int -> labeled_prog
= fun pgm n ->
  match pgm with
  | [] -> []
  | hd::tl ->
  	let decl = gen_hole_decl hd n in
  	decl::(gen_hole_pgm tl n)