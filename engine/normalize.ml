open Lang
open Util
(*****************Constant propagation***************************)
let rec constant_exp : exp -> exp
= fun exp ->
  match exp with
  | ADD (e1,e2) ->
    let e1 = constant_exp e1 in
    let e2 = constant_exp e2 in
    begin match e1,e2 with
    |Const n1,Const n2 -> Const(n1+n2)
    |Const n1,_ -> if(n1=0) then e2 else ADD(e1,e2)
    |_,Const n2 -> if(n2=0) then e1 else ADD(e1,e2)
    |_ -> ADD(e1,e2)
    end 
  | MUL (e1,e2) -> 
    let e1 = constant_exp e1 in
    let e2 = constant_exp e2 in
    begin match e1,e2 with
    |Const n1,Const n2 -> Const(n1*n2)
    |Const n1,_ -> if(n1=0) then Const 0 else if (n1=1) then e2 else MUL(e1,e2)
    |_,Const n2 -> if(n2=0) then Const 0 else if (n2=1) then e1 else MUL(e1,e2)
    |_ -> MUL(e1,e2)
    end
  | OR (e1,e2) -> 
    let e1 = constant_exp e1 in
    let e2 = constant_exp e2 in
    if(e1=FALSE || e2=FALSE) then FALSE else if(e1=TRUE || e2=TRUE) then TRUE else OR(e1,e2) 
  | AND (e1,e2) -> 
    let e1 = constant_exp e1 in
    let e2 = constant_exp e2 in
    if(e1=TRUE && e2=TRUE) then TRUE else if (e1=FALSE || e2=FALSE) then FALSE else AND(e1,e2)
  | EQUAL (e1,e2) -> 
    let e1 = constant_exp e1 in
    let e2 = constant_exp e2 in
    if (e1=e2) then TRUE else EQUAL(e1,e2)
  | NOTEQ (e1,e2) -> 
    let e1 = constant_exp e1 in
    let e2 = constant_exp e2 in
    if (e1!=e2) then TRUE else NOTEQ(e1,e2)
  | SUB (e1,e2) -> 
    let e1 = constant_exp e1 in
    let e2 = constant_exp e2 in
    if(e1=e2) then Const 0 
    else
    begin match e1,e2 with
    |Const n1,Const n2 -> Const(n1-n2)
    |_,Const n2 -> if(n2=0) then e1 else SUB(e1,e2)
    |_ -> SUB(e1,e2)
    end 
  | DIV (e1,e2) ->
    let e1 = constant_exp e1 in
    let e2 = constant_exp e2 in
    if(e2=Const 0) then DIV(e1,e2)
    else if(e1=e2) then Const 1 
    else
    begin match e1,e2 with
    |Const n1,Const n2 -> Const(n1 / n2)
    |_,Const n2 -> if(n2=1) then e1 else DIV(e1,e2)
    |_ -> DIV(e1,e2)
    end 
  | LESS (e1,e2) ->
    let e1 = constant_exp e1 in
    let e2 = constant_exp e2 in
    if(e1=e2) then FALSE
    else
    begin match e1,e2 with
    |Const n1,Const n2 -> if(n1<n2) then TRUE else FALSE
    |_ -> LESS(e1,e2)
    end 
  | LARGER (e1,e2) ->
    let e1 = constant_exp e1 in
    let e2 = constant_exp e2 in
    if(e1=e2) then FALSE
    else
    begin match e1,e2 with
    |Const n1,Const n2 -> if(n1>n2) then TRUE else FALSE
    |_ -> LARGER(e1,e2)
    end 
  | LESSEQ (e1,e2) -> 
    let e1 = constant_exp e1 in
    let e2 = constant_exp e2 in
    if(e1=e2) then TRUE
    else
    begin match e1,e2 with
    |Const n1,Const n2 -> if(n1<=n2) then TRUE else FALSE
    |_ -> LESSEQ(e1,e2)
    end
  | LARGEREQ (e1,e2) ->
    let e1 = constant_exp e1 in
    let e2 = constant_exp e2 in
    if(e1=e2) then TRUE
    else
    begin match e1,e2 with
    |Const n1,Const n2 -> if(n1>=n2) then TRUE else FALSE
    |_ -> LARGEREQ(e1,e2)
    end 
  | EApp (e1,e2) -> 
    let e1 = constant_exp e1 in
    let e2 = constant_exp e2 in
    EApp (e1,e2)
  | MOD (e1,e2) -> 
    let e1 = constant_exp e1 in
    let e2 = constant_exp e2 in
    if(e2=Const 0) then MOD(e1,e2)
    else if(e1=e2) then Const 0 
    else
    begin match e1,e2 with
    |Const n1,Const n2 -> Const(n1 mod n2)
    |_,Const n2 -> if(n2=1) then Const 0 else MOD(e1,e2)
    |_ -> MOD(e1,e2)
    end
  | AT (e1,e2) -> 
    let e1 = constant_exp e1 in
    let e2 = constant_exp e2 in
    begin match e1,e2 with
    |EList l1,EList l2 -> EList (l1@l2)
    |EList l1,_ -> if(l1=[]) then e2 else AT (e1,e2)
    |_,EList l2 -> if(l2=[]) then e1 else AT (e1,e2)
    |_ -> AT(e1,e2)
    end
  | DOUBLECOLON (e1,e2) ->
    let e1 = constant_exp e1 in
    let e2 = constant_exp e2 in
    begin match e2 with
    |EList l1 -> if(l1=[]) then e1 else DOUBLECOLON(e1,e2)
    |_ -> DOUBLECOLON(e1,e2)
    end 
  | MINUS e -> 
    let e = constant_exp e in
    begin match e with
    |Const 0 -> Const 0
    |_ -> MINUS e
    end
  | NOT e -> 
    let e = constant_exp e in
    if(e=TRUE) then FALSE else if (e=FALSE) then TRUE else NOT e
  | EFun (a,e) -> 
    let e = constant_exp e in
    EFun (a,e)
  | IF (e1,e2,e3) -> 
    let e1 = constant_exp e1 in
    let e2 = constant_exp e2 in
    let e3 = constant_exp e3 in
    begin match e1 with
    |TRUE -> e2
    |FALSE -> e3
    |_ -> IF(e1,e2,e3)
    end
  | ELet (x,is_rec,args,t,e1,e2) -> 
    let e1 = constant_exp e1 in
    let e2 = constant_exp e2 in
    ELet(x,is_rec,args,t,e1,e2)
  | ECtor (x,exps) ->
    ECtor(x,list_map constant_exp exps)
  | EList (exps) -> 
    EList (list_map constant_exp exps)
  | ETuple (exps) -> 
    ETuple (list_map constant_exp exps)
  | EMatch (exp,branches) ->
    let exp = constant_exp exp in
    let (pat_list,exp_list) = list_split branches in
    let exp_list = list_map constant_exp exp_list in
    let branches = list_combine pat_list exp_list in
    EMatch (exp,branches) 
  |_ -> exp

let constant_decl : decl -> decl
= fun decl ->
	match decl with
  | DExcept _ -> decl
	| DData _ -> decl
  | DLet (x,is_rec,args,typ,exp) -> 
    let exp = constant_exp exp in
    DLet (x,is_rec,args,typ,exp)

let constant_pgm : prog -> prog 
= fun pgm -> list_map constant_decl pgm

(*****************Program Reordering***************************)

let rec arg_num : exp -> int
= fun e ->
	match e with
	| ADD (e1,e2) 
  | MUL (e1,e2) 
  | OR (e1,e2) 
  | AND (e1,e2) 
  | EQUAL (e1,e2) 
  | NOTEQ (e1,e2) 
  | SUB (e1,e2) 
  | DIV (e1,e2)
  | LESS (e1,e2) 
  | LARGER (e1,e2) 
  | LESSEQ (e1,e2) 
  | LARGEREQ (e1,e2)
  | EApp (e1,e2)  
  | MOD (e1,e2) 
  | AT (e1,e2) 
  | DOUBLECOLON (e1,e2) -> (arg_num e1) + (arg_num e2)
  | MINUS e 
  | NOT e 
  | EFun (_,e) -> (arg_num e)
  | IF (e1,e2,e3) -> (arg_num e1) + (arg_num e2) + (arg_num e3)
  | ELet (x,is_rec,args,t,e1,e2) -> (arg_num e1) + (arg_num e2)
  | ECtor (_,exps) 
  | EList (exps) 
  | ETuple (exps) -> list_fold (fun e r -> (arg_num e) + r) exps 0
  | EMatch (exp,branches) -> 
  	let (pats,exps) = list_split branches in 
  	(arg_num exp)+ (list_fold (fun e r -> (arg_num e) + r ) exps 0)
	|_ -> 1

let compare_exp : exp -> exp -> bool
= fun e1 e2 ->
	match (e1,e2) with
	| (EVar x1,EVar x2) -> x1 > x2
  | (Const x1,Const x2) -> x1 > x2
  | (EVar _,_) -> true
  | (_,EVar _) -> false
  | (Hole _,_) -> true
  | (_,Hole _) -> false
  | _ -> ((arg_num e1) >= (arg_num e2))

let rec reorder_exp : exp -> exp
= fun exp ->
	match exp with	
  | ADD (e1,e2) -> if compare_exp e1 e2 then ADD (reorder_exp e1,reorder_exp e2) else ADD (reorder_exp e2,reorder_exp e1)
  | MUL (e1,e2) -> if compare_exp e1 e2 then MUL (reorder_exp e1,reorder_exp e2) else MUL (reorder_exp e2,reorder_exp e1)
  | OR (e1,e2) -> if compare_exp e1 e2 then OR (reorder_exp e1,reorder_exp e2) else OR (reorder_exp e2,reorder_exp e1)
  | AND (e1,e2) -> if compare_exp e1 e2 then AND (reorder_exp e1,reorder_exp e2) else AND (reorder_exp e2,reorder_exp e1)
  | EQUAL (e1,e2) -> if compare_exp e1 e2 then EQUAL (reorder_exp e1,reorder_exp e2) else EQUAL (reorder_exp e2,reorder_exp e1)
  | NOTEQ (e1,e2) -> if compare_exp e1 e2 then NOTEQ (reorder_exp e1,reorder_exp e2) else NOTEQ (reorder_exp e2,reorder_exp e1)
  | SUB (e1,e2) -> SUB (reorder_exp e1,reorder_exp e2)
  | DIV (e1,e2) -> DIV (reorder_exp e1,reorder_exp e2)
  | MOD (e1,e2) -> MOD (reorder_exp e1,reorder_exp e2)
  | MINUS e -> MINUS (reorder_exp e)
  | NOT e -> NOT (reorder_exp e)
  | LESS (e1,e2) -> if compare_exp e1 e2 then LESS (reorder_exp e1,reorder_exp e2) else LESS (reorder_exp e2,reorder_exp e1)
  | LARGER (e1,e2) -> reorder_exp (LESS (e2,e1))
  | LESSEQ (e1,e2) -> reorder_exp (OR (LESS (e1,e2),EQUAL(e1,e2)))
  | LARGEREQ (e1,e2) -> reorder_exp (OR (LESS (e2,e1),EQUAL(e1,e2)))
  | EApp (e1,e2) -> EApp (reorder_exp e1,reorder_exp e2)
  | EFun (arg,e) -> EFun (arg, reorder_exp e)
  | ELet (x,is_rec,args,t,e1,e2) -> ELet (x,is_rec,args,t,reorder_exp e1,reorder_exp e2)
  | ECtor (id,exps) -> ECtor(id,(list_map reorder_exp exps))
  | EList (exps) -> EList (list_map reorder_exp exps)
  | ETuple (exps) -> ETuple (list_map reorder_exp exps)
  | EMatch (exp,branches) ->
    let (pat_list,exp_list) = list_split branches in
    let exp_list = list_map reorder_exp exp_list in
    let branches = list_combine pat_list exp_list in
    EMatch (reorder_exp exp,branches)
  | IF (e1,e2,e3) -> IF (reorder_exp e1,reorder_exp e2,reorder_exp e3)
  | AT (e1,e2) -> AT (reorder_exp e1,reorder_exp e2)
  | DOUBLECOLON (e1,e2) -> DOUBLECOLON (reorder_exp e1,reorder_exp e2)
  |_ -> exp

let reorder_decl : decl -> decl
= fun decl ->
	match decl with
  | DExcept _ -> decl
	| DData _ -> decl
  | DLet (x,is_rec,args,typ,exp) -> 
    let exp = reorder_exp exp in
    DLet (x,is_rec,args,typ,exp)

let reorder_pgm : prog -> prog 
= fun pgm -> list_map reorder_decl pgm

let normalize : prog -> prog
= fun pgm ->
  let constant_func = (fun x -> constant_pgm x) in
	let normalize_func = (fun x ->reorder_pgm x) in
  let pgm = fix constant_func pgm in
	fix normalize_func pgm
