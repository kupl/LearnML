(* labeling program *)
open Lang
open Util
open Label_lang

(* Tag a label to program AST *)
let rec labeling_exp : exp -> labeled_exp
= fun exp ->
	match exp with
	| EUnit -> (new_label (), EUnit)
	| Const n -> (new_label (), Const n)
	| TRUE -> (new_label (), TRUE)
	| FALSE -> (new_label (), FALSE)
	| String str -> (new_label (), String str)
	| EVar x -> (new_label (), EVar x)
	| EList es -> (new_label (), EList (labeling_explist es))
	| ETuple es -> (new_label (), ETuple (labeling_explist es))
	| ECtor (x, es) -> (new_label (), ECtor (x, labeling_explist es))
	| ADD (e1, e2) -> (new_label (), ADD (labeling_exp e1, labeling_exp e2))
	| SUB (e1, e2) -> (new_label (), SUB (labeling_exp e1, labeling_exp e2))
	| MUL (e1, e2) -> (new_label (), MUL (labeling_exp e1, labeling_exp e2))
	| DIV (e1, e2) -> (new_label (), DIV (labeling_exp e1, labeling_exp e2))
	| MOD (e1, e2) -> (new_label (), MOD (labeling_exp e1, labeling_exp e2))
	| OR (e1, e2) -> (new_label (), OR (labeling_exp e1, labeling_exp e2))
	| AND (e1, e2) -> (new_label (), AND (labeling_exp e1, labeling_exp e2))
	| LESS (e1, e2) -> (new_label (), LESS (labeling_exp e1, labeling_exp e2))
	| LARGER (e1, e2) -> (new_label (), LARGER (labeling_exp e1, labeling_exp e2))
	| EQUAL (e1, e2) -> (new_label (), EQUAL (labeling_exp e1, labeling_exp e2))
	| NOTEQ (e1, e2) -> (new_label (), NOTEQ (labeling_exp e1, labeling_exp e2))
	| LESSEQ (e1, e2) -> (new_label (), LESSEQ (labeling_exp e1, labeling_exp e2))
	| LARGEREQ (e1, e2) -> (new_label (), LARGEREQ (labeling_exp e1, labeling_exp e2))
	| EApp (e1, e2) -> (new_label (), EApp (labeling_exp e1, labeling_exp e2))
	| AT (e1, e2) -> (new_label (), AT (labeling_exp e1, labeling_exp e2))
	| DOUBLECOLON (e1, e2) -> (new_label (), DOUBLECOLON (labeling_exp e1, labeling_exp e2))
	| STRCON (e1,e2) -> (new_label (),STRCON (labeling_exp e1,labeling_exp e2))
  | MINUS e -> (new_label (), MINUS (labeling_exp e))
	| NOT e -> (new_label (), NOT (labeling_exp e))
	| EFun (x, e) -> (new_label (), EFun (x, labeling_exp e))
	| ELet (f, is_rec, args, typ, e1, e2) -> (new_label (), ELet (f, is_rec, args, typ, labeling_exp e1, labeling_exp e2))
	| EBlock (is_rec, ds, e2) -> 
		let ds = List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, labeling_exp e)) ds in
		(new_label (), EBlock (is_rec, ds, labeling_exp e2))
	| EMatch (e, bs) -> 
		let (ps, es) = List.split bs in
		(new_label (), EMatch (labeling_exp e, List.combine ps (labeling_explist es)))
	| IF (e1, e2, e3) -> (new_label (), IF (labeling_exp e1, labeling_exp e2, labeling_exp e3))
	| Hole n -> (new_label (), Hole n)
  | Raise e -> (new_label (),Raise (labeling_exp e))

and labeling_explist : exp list -> labeled_exp list
= fun es -> List.fold_left (fun acc x -> acc@[labeling_exp x]) [] es

let rec labeling_decl : decl -> labeled_decl
= fun decl ->
	match decl with
  | DExcept t -> DExcept t
  | DEqn (x, typ) -> DEqn (x, typ)
	| DData (id, ctors) -> DData (id, ctors)
	| DLet (f, is_rec, args, typ, e) -> DLet (f, is_rec, args, typ, labeling_exp e)
	| DBlock (is_rec, ds) -> 
		let ds = List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, labeling_exp e)) ds in
		DBlock (is_rec, ds)
	| TBlock ds -> TBlock (List.map (labeling_decl) ds)

let labeling_prog : prog -> labeled_prog
= fun pgm -> List.fold_left (fun acc x -> acc@[labeling_decl x]) [] pgm

(* labeld AST -> AST *)
let rec unlabeling_exp : labeled_exp -> exp
= fun (l, exp) ->
	match exp with
	| EUnit -> EUnit
	| Const n -> Const n
	| TRUE -> TRUE
	| FALSE -> FALSE
	| String str -> String str
	| EVar x -> EVar x
	| EList es -> EList (unlabeling_explist es)
	| ETuple es -> ETuple (unlabeling_explist es)
	| ECtor (x, es) -> ECtor (x, unlabeling_explist es)
	| ADD (e1, e2) -> ADD (unlabeling_exp e1, unlabeling_exp e2)
	| SUB (e1, e2) -> SUB (unlabeling_exp e1, unlabeling_exp e2)
	| MUL (e1, e2) -> MUL (unlabeling_exp e1, unlabeling_exp e2)
	| DIV (e1, e2) -> DIV (unlabeling_exp e1, unlabeling_exp e2)
	| MOD (e1, e2) -> MOD (unlabeling_exp e1, unlabeling_exp e2)
	| OR (e1, e2) -> OR (unlabeling_exp e1, unlabeling_exp e2)
	| AND (e1, e2) -> AND (unlabeling_exp e1, unlabeling_exp e2)
	| LESS (e1, e2) -> LESS (unlabeling_exp e1, unlabeling_exp e2)
	| LARGER (e1, e2) -> LARGER (unlabeling_exp e1, unlabeling_exp e2)
	| EQUAL (e1, e2) -> EQUAL (unlabeling_exp e1, unlabeling_exp e2)
	| NOTEQ (e1, e2) -> NOTEQ (unlabeling_exp e1, unlabeling_exp e2)
	| LESSEQ (e1, e2) -> LESSEQ (unlabeling_exp e1, unlabeling_exp e2)
	| LARGEREQ (e1, e2) -> LARGEREQ (unlabeling_exp e1, unlabeling_exp e2)
	| EApp (e1, e2) -> EApp (unlabeling_exp e1, unlabeling_exp e2)
	| AT (e1, e2) -> AT (unlabeling_exp e1, unlabeling_exp e2)
	| DOUBLECOLON (e1, e2) -> DOUBLECOLON (unlabeling_exp e1, unlabeling_exp e2)
	| STRCON (e1,e2) -> STRCON (unlabeling_exp e1,unlabeling_exp e2)
  | MINUS e -> MINUS (unlabeling_exp e)
	| NOT e -> NOT (unlabeling_exp e)
	| EFun (x, e) -> EFun (x, unlabeling_exp e)
	| ELet (f, is_rec, args, typ, e1, e2) -> ELet (f, is_rec, args, typ, unlabeling_exp e1, unlabeling_exp e2)
	| EBlock (is_rec, ds, e2) -> 
		let ds = List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, unlabeling_exp e)) ds in
		EBlock (is_rec, ds, unlabeling_exp e2)
	| EMatch (e, bs) ->
		let (ps, es) = List.split bs in
		EMatch (unlabeling_exp e, List.combine ps (unlabeling_explist es))
	| IF (e1, e2, e3) -> IF (unlabeling_exp e1, unlabeling_exp e2, unlabeling_exp e3)
	| Hole n -> Hole n
  | Raise e -> Raise (unlabeling_exp e)

and unlabeling_explist : labeled_exp list -> exp list
= fun es -> List.map unlabeling_exp es

let rec unlabeling_decl : labeled_decl -> decl
= fun l_decl ->
	match l_decl with
  | DExcept t -> DExcept t
  | DEqn (x, typ) -> DEqn (x, typ)
	| DData (x, ctors) -> DData (x, ctors)
	| DLet (f, is_rec, args, typ, e) -> DLet (f, is_rec, args, typ ,unlabeling_exp e)
	| DBlock (is_rec, ds) -> 
		let ds = List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, unlabeling_exp e)) ds in
		DBlock (is_rec, ds)
	| TBlock ds -> TBlock (List.map (unlabeling_decl) ds)

let unlabeling_prog : labeled_prog -> prog
= fun l_pgm -> List.fold_left (fun acc x -> acc@[unlabeling_decl x]) [] l_pgm

(*** generate hole in error location *****)
let rec gen_hole_exp : int -> labeled_exp -> labeled_exp
= fun n (label, exp) ->
	if label = n then labeling_exp (gen_hole ()) else 
	match exp with 
	| EList es -> (label, EList (gen_hole_explist n es))
	| ETuple es -> (label, ETuple (gen_hole_explist n es))
	| ECtor (x, es) -> (label, ECtor (x,gen_hole_explist n es))
	| ADD (e1, e2) -> (label, ADD (gen_hole_exp n e1, gen_hole_exp n e2))
	| SUB (e1, e2) -> (label, SUB (gen_hole_exp n e1, gen_hole_exp n e2))
	| MUL (e1, e2) -> (label, MUL (gen_hole_exp n e1, gen_hole_exp n e2))
	| DIV (e1, e2) -> (label, DIV (gen_hole_exp n e1, gen_hole_exp n e2))
	| MOD (e1, e2) -> (label, MOD (gen_hole_exp n e1, gen_hole_exp n e2))
	| OR (e1, e2) -> (label, OR (gen_hole_exp n e1, gen_hole_exp n e2))
	| AND (e1, e2) -> (label, AND (gen_hole_exp n e1, gen_hole_exp n e2))
	| LESS (e1, e2) -> (label, LESS (gen_hole_exp n e1, gen_hole_exp n e2))
	| LARGER (e1, e2) -> (label, LARGER (gen_hole_exp n e1, gen_hole_exp n e2))
	| EQUAL (e1, e2) -> (label, EQUAL (gen_hole_exp n e1, gen_hole_exp n e2))
	| NOTEQ (e1, e2) -> (label, NOTEQ (gen_hole_exp n e1, gen_hole_exp n e2))
	| LESSEQ (e1, e2) -> (label, LESSEQ (gen_hole_exp n e1, gen_hole_exp n e2))
	| LARGEREQ (e1, e2) -> (label, LARGEREQ (gen_hole_exp n e1, gen_hole_exp n e2))
	| EApp (e1, e2) -> (label, EApp (gen_hole_exp n e1, gen_hole_exp n e2))
	| AT (e1, e2) -> (label, AT (gen_hole_exp n e1, gen_hole_exp n e2))
	| DOUBLECOLON (e1, e2) -> (label, DOUBLECOLON (gen_hole_exp n e1, gen_hole_exp n e2))
	| MINUS e -> (label, MINUS (gen_hole_exp n e))
	| NOT e -> (label, NOT (gen_hole_exp n e))
	| EFun (x, e) -> (label, EFun (x, gen_hole_exp n e))
	| ELet (f, is_rec, args, typ, e1, e2) -> (label, ELet (f, is_rec, args, typ, gen_hole_exp n e1, gen_hole_exp n e2))
	| EBlock (is_rec, ds, e2) -> 
		let ds = List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, gen_hole_exp n e)) ds in
		(label, EBlock (is_rec, ds, gen_hole_exp n e2))
	| EMatch (e, bs) ->
		let (ps, es) = List.split bs in
		(label, EMatch (gen_hole_exp n e, List.combine ps (gen_hole_explist n es)))
	| IF (e1, e2, e3) -> (label, IF (gen_hole_exp n e1, gen_hole_exp n e2, gen_hole_exp n e3))
	| _ -> (label, exp)

and gen_hole_explist : int -> labeled_exp list -> labeled_exp list
= fun n es -> List.map (gen_hole_exp n) es

let rec gen_hole_decl : int -> labeled_decl -> labeled_decl
= fun n l_decl ->
	match l_decl with
	| DLet (f, is_rec, args, typ, e) -> DLet (f, is_rec, args, typ, gen_hole_exp n e)
	| DBlock (is_rec, ds) -> 
		let ds = List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, gen_hole_exp n e)) ds in
		DBlock (is_rec, ds)
	| _ -> l_decl

let rec gen_hole_pgm : int -> labeled_prog -> labeled_prog
= fun n l_pgm -> List.fold_left (fun acc x -> acc@[gen_hole_decl n x]) [] l_pgm
