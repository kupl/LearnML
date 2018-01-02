open Lang
open Util
(*
 ******************************************************
 	Code for extract components from correct code
 ******************************************************
*)

let rec find_pat_component: pat -> pat BatSet.t
= fun p  ->
	match p with
	| PCtor (x,lst) -> (BatSet.singleton (PCtor(x,lst))) 
	| Pats lst -> 
		let (exps,pats) = (pat_list_to_component lst (BatSet.empty,BatSet.empty)) in
		if (lst = []) then (BatSet.add (Pats []) pats) 
		else BatSet.add (Pats (lst)) pats
	| PInt n -> (BatSet.singleton (PInt (n)))
	| PVar x -> (BatSet.singleton (PVar (x)))
	| PBool b -> (BatSet.singleton (PBool (b)))
	| PList lst -> (BatSet.singleton (PList (lst)))
	| PTuple lst -> (BatSet.singleton (PTuple (lst))) 
	| PUnder -> (BatSet.singleton (PUnder))
	| PCons lst -> (BatSet.singleton (PCons (lst)))
	| _ -> raise (Failure "PHole error!") 

and find_component : exp-> components -> components
= fun e (exps,pats)->
	match e with
	| Const n -> (BatSet.add (Const n) exps,pats)
	| String id -> (BatSet.add (String id) exps,pats)
	| ADD (e1,e2) ->
	 let (es1,ps1) = find_component e1 (exps,pats) in
	 let (es2,ps2) = find_component e2 (es1,ps1) in
	 (BatSet.add (ADD (Hole (0),Hole (0))) es2, ps2)
	| SUB (e1,e2) ->
	 let (es1,ps1) = find_component e1 (exps,pats) in
	 let (es2,ps2) = find_component e2 (es1,ps1) in
	 (BatSet.add (SUB (Hole (0),Hole (0))) es2, ps2)
	| MUL (e1,e2) ->
	 let (es1,ps1) = find_component e1 (exps,pats) in
	 let (es2,ps2) = find_component e2 (es1,ps1) in
	 (BatSet.add (MUL (Hole (0),Hole (0))) es2, ps2)
	| DIV (e1,e2) ->
	 let (es1,ps1) = find_component e1 (exps,pats) in
	 let (es2,ps2) = find_component e2 (es1,ps1) in
	 (BatSet.add (DIV (Hole (0),Hole (0))) es2, ps2)
	| MOD (e1,e2) ->
	 let (es1,ps1) = find_component e1 (exps,pats) in
	 let (es2,ps2) = find_component e2 (es1,ps1) in
	 (BatSet.add (MOD (Hole (0),Hole (0))) es2, ps2)
	| TRUE -> (BatSet.add TRUE exps,pats)
	| FALSE -> (BatSet.add FALSE exps,pats)
	| NOT e1 -> 
	 let (es1,ps1) = find_component e1 (exps,pats) in
	 (BatSet.add (NOT (Hole (0))) es1, ps1)
	| MINUS e1 -> 
	 let (es1,ps1) = find_component e1 (exps,pats) in
	 (BatSet.add (MINUS (Hole (0))) es1, ps1)
	| OR (e1,e2) ->
	 let (es1,ps1) = find_component e1 (exps,pats) in
	 let (es2,ps2) = find_component e2 (es1,ps1) in
	 (BatSet.add (OR (Hole (0),Hole (0))) es2, ps2)
	| AND (e1,e2) ->
	 let (es1,ps1) = find_component e1 (exps,pats) in
	 let (es2,ps2) = find_component e2 (es1,ps1) in
	 (BatSet.add (AND (Hole (0),Hole (0))) es2, ps2)
	| LESS (e1,e2) ->
	 let (es1,ps1) = find_component e1 (exps,pats) in
	 let (es2,ps2) = find_component e2 (es1,ps1) in
	 (BatSet.add (LESS (Hole (0),Hole (0))) es2, ps2)
	| LARGER (e1,e2) ->
	 let (es1,ps1) = find_component e1 (exps,pats) in
	 let (es2,ps2) = find_component e2 (es1,ps1) in
	 (BatSet.add (LARGER (Hole (0),Hole (0))) es2, ps2)
	| EQUAL (e1,e2) ->
	 let (es1,ps1) = find_component e1 (exps,pats) in
	 let (es2,ps2) = find_component e2 (es1,ps1) in
	 (BatSet.add (EQUAL (Hole (0),Hole (0))) es2, ps2)
	| NOTEQ (e1,e2) ->
	 let (es1,ps1) = find_component e1 (exps,pats) in
	 let (es2,ps2) = find_component e2 (es1,ps1) in
	 (BatSet.add (NOTEQ (Hole (0),Hole (0))) es2, ps2)
	| LESSEQ (e1,e2) ->
	 let (es1,ps1) = find_component e1 (exps,pats) in
	 let (es2,ps2) = find_component e2 (es1,ps1) in
	 (BatSet.add (LESSEQ (Hole (0),Hole (0))) es2, ps2)
	| LARGEREQ (e1,e2) ->
	 let (es1,ps1) = find_component e1 (exps,pats) in
	 let (es2,ps2) = find_component e2 (es1,ps1) in
	 (BatSet.add (LARGEREQ (Hole (0),Hole (0))) es2, ps2)
	| EVar x -> (exps,pats)
	| EApp (e1,e2) ->
	 let (es1,ps1) = find_component e1 (exps,pats) in
	 let (es2,ps2) = find_component e2 (es1,ps1) in
	 (BatSet.add (EApp (Hole (0),Hole (0))) es2, ps2)
	| IF (e1,e2,e3) ->
	 let (es1,ps1) = find_component e1 (exps,pats) in
	 let (es2,ps2) = find_component e2 (es1,ps1) in
	 let (es3,ps3) = find_component e3 (es2,ps2) in
	 (BatSet.add (IF (Hole (0),Hole (0),Hole (0))) es3, ps3)
	| AT (e1,e2) ->
	 let (es1,ps1) = find_component e1 (exps,pats) in
	 let (es2,ps2) = find_component e2 (es1,ps1) in
	 (BatSet.add (AT (Hole (0),Hole (0))) es2, ps2)
	| DOUBLECOLON (e1,e2) ->
	 let (es1,ps1) = find_component e1 (exps,pats) in
	 let (es2,ps2) = find_component e2 (es1,ps1) in
	 (BatSet.add (DOUBLECOLON (Hole (0),Hole (0))) es2, ps2)
	| EList lst ->
		let (es1,ps1) = exp_list_to_component lst (exps,pats) in
			if (lst = []) then (BatSet.add (EList []) es1,ps1)
			else ((BatSet.add (EList (change_list_to_hole lst))) es1,ps1)
	| ETuple lst ->
		let (es1,ps1) = exp_list_to_component lst (exps,pats) in
			if (lst = []) then (BatSet.add (ETuple []) es1,ps1)
			else ((BatSet.add (ETuple (change_list_to_hole lst))) es1,ps1)
	| ELet (f,is_rec,xs,t,e1,e2) ->
	 let (es1,ps1) = find_component e1 (exps,pats) in
	 let (es2,ps2) = find_component e2 (es1,ps1) in
	 (BatSet.union (BatSet.union (BatSet.add (ELet (f,is_rec,xs,t,Hole (0),Hole (0))) exps) es1) es2,BatSet.union (BatSet.union pats ps1) ps2)
	| EFun (arg,e1) -> 
	 let (es1,ps1) = find_component e1 (exps,pats) in
	 ((BatSet.union (BatSet.add (EFun (arg,Hole (0))) exps) es1) , (BatSet.union pats ps1))

	| ECtor (x,lst) ->
		let (es1,ps1) = exp_list_to_component lst (exps,pats) in
			if (lst = []) then (BatSet.add (ECtor (x,[])) es1,ps1)
			else (BatSet.add (ECtor (x,change_list_to_hole lst)) es1,ps1)
	| EMatch (e,lst) ->
	 let (pl,el) = List.split lst in
	 let (es,ps) = exp_list_to_component el (exps,pats) in
	 let (es,ps) = pat_list_to_component pl (es,ps) in
	 let (es,ps) = find_component e (es,ps) in
	 (BatSet.add (EMatch (Hole (0),change_branch_list_to_hole lst)) es, ps)
	|_ -> raise(Failure "Extract component error")

and exp_list_to_component exps (exp_lst,pat_lst) 
= match exps with
	[] -> (exp_lst,pat_lst)
	| hd::tl -> 
		let (e1,p1) = find_component hd (exp_lst,pat_lst) in
		exp_list_to_component tl (e1,p1)
and pat_list_to_component lst (exp_lst,pat_lst)
= match lst with             
	[] -> (exp_lst,pat_lst) 
	| hd::tl -> pat_list_to_component tl (exp_lst,(BatSet.union (find_pat_component hd) pat_lst))
and change_list_to_hole exps
= match exps with
	[] -> []
	|hd::tl -> Hole (0) :: change_list_to_hole tl
and change_branch_list_to_hole brs
= match brs with
	[] -> []
	|hd::tl -> (gen_pat_hole(),Hole (0)) :: change_branch_list_to_hole tl

let extracts : decl -> components -> components
= fun decl (exps,pats) ->
	match decl with
	| DData _ -> (exps,pats)
	| DLet (x,is_rec,args,typ,exp) ->
		find_component exp (exps,pats)

let extract_component : prog -> components
= fun decls -> (list_fold extracts decls (BatSet.empty,BatSet.empty))