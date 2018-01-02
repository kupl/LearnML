open Lang
open Util

let is_hole : exp -> bool
= fun exp ->
	match exp with
	| Hole _ -> true
	|_ -> false

let rec pat_is_hole : pat -> bool -> bool
= fun pat b ->
	match pat with
    | PInt n -> true && b 
    | PVar id -> true && b 
    | PBool b -> true && b 
    | PUnder -> true && b 
    | PCtor (id,pl) -> list_fold pat_is_hole pl b
    | Pats pl
    | PList pl
    | PTuple pl
    | PCons pl -> list_fold pat_is_hole pl b
    | PHole n -> false

let rec exp_is_closed : exp -> bool -> bool
= fun exp b ->
	match exp with
	| Const n -> b && true
	| TRUE -> b && true
	| FALSE -> b && true
	| String x-> b && true
	| EVar x -> b && true
	| ADD (e1,e2) 
	| SUB (e1,e2)
	| MUL (e1,e2)
	| DIV (e1,e2)
	| MOD (e1,e2)
	| OR (e1,e2)
	| AND (e1,e2)	
	| LESS (e1,e2)
	| LARGER (e1,e2)
	| EQUAL (e1,e2)
	| NOTEQ (e1,e2)
	| LESSEQ (e1,e2)
	| LARGEREQ (e1,e2) 
	| AT (e1,e2) 
	| DOUBLECOLON (e1,e2) ->
		let b1 = (exp_is_closed e1 b) in
		let b2 = (exp_is_closed e2 b1) in
		b2
	| ELet (_,_,_,_,e1,e2) 
	| EApp (e1,e2) ->
		let b1 = (exp_is_closed e1 b) in
		let b2 = (exp_is_closed e2 b1) in
		b2
	| NOT e1 -> (exp_is_closed e1 b)
  | MINUS e1 -> (exp_is_closed e1 b)
	| IF (e1,e2,e3) ->
		let b1 = (exp_is_closed e1 b) in
		let b2 = (exp_is_closed e2 b1) in
		(exp_is_closed e3 b2)
	| ECtor (x,l) -> list_fold exp_is_closed l b
	| EList l 
	| ETuple l-> list_fold exp_is_closed l b
	| EFun (a,e) -> (exp_is_closed e b)
	| EMatch (e,bl) ->
		let (pl,el) = List.split bl in
		let pb = list_fold pat_is_hole pl b in
		let eb = list_fold exp_is_closed el pb in
		exp_is_closed e eb
	| Hole (_) -> false

let check_closed : decl -> bool -> bool
= fun decl b ->
	match decl with
	| DData _ -> b
	| DLet (x,is_rec,args,typ,exp) ->
		exp_is_closed exp b

let is_closed : prog -> bool
= fun decls -> list_fold check_closed decls true

let rec appify : exp -> exp list -> exp
= fun exp exp_list ->
	match exp_list with
	[] -> exp
	|hd::tl -> appify (EApp(exp,hd)) tl

(*let rec is_closed : exp -> bool
= fun exp ->
	match exp with
	| Const n -> true
	| String id -> true
	| TRUE -> true
	| FALSE -> true
	| EVar id -> true
	| ADD (e1,e2) -> (is_closed e1) && (is_closed e2)
	| SUB (e1,e2) -> (is_closed e1) && (is_closed e2)
	| MUL (e1,e2) -> (is_closed e1) && (is_closed e2)
	| DIV (e1,e2) -> (is_closed e1) && (is_closed e2)
	| MOD (e1,e2) -> (is_closed e1) && (is_closed e2)
	| AND (e1,e2) -> (is_closed e1) && (is_closed e2)
	| OR (e1,e2) -> (is_closed e1) && (is_closed e2)
	| LESS (e1,e2) -> (is_closed e1) && (is_closed e2)
	| LESSEQ (e1,e2) -> (is_closed e1) && (is_closed e2)
	| LARGER (e1,e2) -> (is_closed e1) && (is_closed e2)
	| LARGEREQ (e1,e2) -> (is_closed e1) && (is_closed e2)
	| EQUAL (e1,e2) -> (is_closed e1) && (is_closed e2)
	| NOTEQ (e1,e2) -> (is_closed e1) && (is_closed e2)
	| DOUBLECOLON (e1,e2) -> (is_closed e1) && (is_closed e2)
	| AT (e1,e2) -> (is_closed e1) && (is_closed e2)
	| NOT e1 -> (is_closed e1)
	| MINUS e1 -> (is_closed e1)
	| EFun (args,e) -> (is_closed e)
	| EApp (e1,e2) -> (is_closed e1)&&(is_closed e2)
	| IF (e1,e2,e3) -> (is_closed e1)&&(is_closed e2)&&(is_closed e3)
	| ELet (id,is_rec,args,t,e1,e2) -> (is_closed e1)&&(is_closed e2)
	| ECtor (id,lst) -> List.for_all is_closed lst
	| EList exp_list -> List.for_all is_closed exp_list
	| ETuple exp_list -> List.for_all is_closed exp_list
	| EMatch (e,branch_list) ->
		let (pat_list,exp_list) = list_split branch_list in
		(List.for_all is_closed exp_list) && (is_closed e)
	| Hole n -> false
	|_ -> raise (Failure ("translate exp"))
*)
let rec translate_exp : exp -> exp
= fun exp ->
	match exp with
	| Const n -> Const n
	| String id -> String id
	| TRUE -> TRUE
	| FALSE -> FALSE
	| EVar id -> EVar id
	| ADD (e1,e2) -> 
		if ((is_hole e1) || (is_hole e2)) then gen_hole() 
		else ADD (translate_exp e1,translate_exp e2)
	| SUB (e1,e2) ->
		if ((is_hole e1) || (is_hole e2)) then gen_hole() 
		else SUB (translate_exp e1,translate_exp e2)
	| MUL (e1,e2) ->
		if ((is_hole e1) && (is_hole e2)) then gen_hole() 
		else MUL (translate_exp e1,translate_exp e2)
	| DIV (e1,e2) ->
		if ((is_hole e1) && (is_hole e2)) then gen_hole() 
		else DIV (translate_exp e1,translate_exp e2)
	| MOD (e1,e2) ->
		if ((is_hole e1) && (is_hole e2)) then gen_hole() 
		else MOD (translate_exp e1,translate_exp e2)
	| OR (e1,e2) ->
		if ((is_hole e1) || (is_hole e2)) then gen_hole() 
		else OR (translate_exp e1,translate_exp e2)
	| AND (e1,e2) ->
		if ((is_hole e1) || (is_hole e2)) then gen_hole() 
		else AND (translate_exp e1,translate_exp e2)
	| LESS (e1,e2) ->
		if ((is_hole e1) || (is_hole e2)) then gen_hole() 
		else LESS (translate_exp e1,translate_exp e2)
	| LARGER (e1,e2) ->
		if ((is_hole e1) || (is_hole e2)) then gen_hole() 
		else LARGER (translate_exp e1,translate_exp e2)
	| EQUAL (e1,e2) ->
		if ((is_hole e1) || (is_hole e2)) then gen_hole() 
		else EQUAL (translate_exp e1,translate_exp e2)
	| NOTEQ (e1,e2) ->
		if ((is_hole e1) || (is_hole e2)) then gen_hole() 
		else NOTEQ (translate_exp e1,translate_exp e2)
	| LESSEQ (e1,e2) ->
		if ((is_hole e1) || (is_hole e2)) then gen_hole() 
		else LESSEQ (translate_exp e1,translate_exp e2)
	| LARGEREQ (e1,e2) ->
		if ((is_hole e1) || (is_hole e2)) then gen_hole() 
		else LARGEREQ (translate_exp e1,translate_exp e2)
	| DOUBLECOLON (e1,e2) ->
		if ((is_hole e1) && (is_hole e2)) then gen_hole() 
		else DOUBLECOLON (translate_exp e1,translate_exp e2)
	| AT (e1,e2) ->
		if ((is_hole e1) && (is_hole e2)) then gen_hole() 
		else AT (translate_exp e1,translate_exp e2)
	| MINUS e1 ->
		if is_hole e1 then gen_hole()
		else MINUS (translate_exp e1)
	| NOT e1 ->
		if is_hole e1 then gen_hole()
		else NOT (translate_exp e1)
	| EFun (args,e) ->
		if is_hole e then gen_hole()
		else EFun (args,(translate_exp e))
	| EApp (e1,e2) ->
		if ((exp_is_closed e1 true)&&(exp_is_closed e2 true)) then EApp (e1,translate_exp e2)
		else gen_hole() 
	| IF (e1,e2,e3) ->
		if (exp_is_closed e1 true) then IF (translate_exp e1, translate_exp e2, translate_exp e3)
		else gen_hole() 
	| ELet (id,is_rec,args,t,e1,e2) ->
		ELet (id,is_rec,args,t,translate_exp e1,translate_exp e2)
	| ECtor (id,lst) ->
		ECtor (id,list_map translate_exp lst)
	| EList exp_list ->
		if ((list_filter (fun e -> not (is_hole e)) exp_list) = []) && (exp_list !=[]) then gen_hole()
		else EList (list_map translate_exp exp_list)
	| ETuple exp_list ->
		if ((list_filter (fun e -> not (is_hole e)) exp_list) = []) && (exp_list !=[]) then gen_hole()
		else EList (list_map translate_exp exp_list)
	| EMatch (e,branch_list) ->
		let (pat_list,exp_list) = list_split branch_list in
		if (((list_filter (fun e -> not (is_hole e)) exp_list) = []) || not (exp_is_closed e true)) then gen_hole()
		else
			let exp_list = list_map translate_exp exp_list in
			let branch_list = list_combine pat_list exp_list in
			EMatch (translate_exp e,branch_list)
	| Hole n -> Hole n
	|_ -> raise (Failure ("translate exp"))

let translate_decl : decl -> decl
= fun decl ->
	match decl with
  | DData _ -> decl
  | DLet (x,is_rec,args,typ,exp) -> 
    let exp = ELet (x, is_rec, args, typ, exp, EVar x) in
    let exp = fix translate_exp exp in
    begin match exp with
	  |ELet (id,is_rec,args,typ,e1,e2) -> DLet (id,is_rec,args,typ,e1)
	  |_ -> raise (Failure "translate decl")
    end

let translate : prog -> prog
= fun decls -> list_map translate_decl decls

