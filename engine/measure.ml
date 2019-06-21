(******************************************************************)
(***** Measure the quality of testcases based on some metrics *****)
(******************************************************************)
open Lang
open Util

(* Exp coverage*)
let run_pgm : prog -> example -> env
= fun pgm (input, output) ->
  let res_var = "__res__" in
  let pgm = pgm@(!grading_pgm) in
  let pgm' = pgm @ [(DLet (BindOne res_var,false,[],fresh_tvar(),(Lang.appify (gen_label(), EVar !Options.opt_entry_func) input)))] in
  Eval.run pgm'

let get_executed_labels : prog -> example -> label BatSet.t
= fun pgm example ->
	Eval.trace_option := true;
  let _ = try run_pgm pgm example with _ -> empty_env in
  Eval.trace_option := false;
  list2set (!Eval.trace_set)

let rec get_exp_labels : lexp -> label BatSet.t
= fun (l, exp) ->
	match exp with 
	| EList es | ECtor (_, es) | ETuple es -> BatSet.add l (List.fold_left (fun set e -> BatSet.union set (get_exp_labels e)) BatSet.empty es)
  | MINUS e | NOT e | EFun (_, e) | Raise e -> BatSet.add l (get_exp_labels e)
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2)
  | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LARGER (e1, e2) | EQUAL (e1, e2) | NOTEQ (e1, e2)
  | LESSEQ (e1, e2) | LARGEREQ (e1, e2) | AT (e1, e2) | DOUBLECOLON (e1, e2) | STRCON (e1, e2)
  | EApp (e1, e2) | ELet (_, _, _, _, e1, e2) -> BatSet.add l (BatSet.union (get_exp_labels e1) (get_exp_labels e2))
	| EBlock (_, ds, e2) -> 
		let r = List.fold_left (fun set (f, is_rec, args, typ, e) -> BatSet.union set (get_exp_labels e)) (get_exp_labels e2) ds in
		BatSet.add l r
	| EMatch (e, bs) ->
		let r = List.fold_left (fun set (p, e) -> BatSet.union set (get_exp_labels e)) (get_exp_labels e) bs in
		BatSet.add l r
	| IF (e1, e2, e3) -> 
		let r = BatSet.union (get_exp_labels e1) (BatSet.union (get_exp_labels e2) (get_exp_labels e3)) in
		BatSet.add l r
  | _ -> BatSet.singleton l

let rec get_decl_labels : decl -> label BatSet.t
= fun decl ->
  match decl with
  | DLet (f, is_rec, args, typ, exp) -> get_exp_labels exp
  | DBlock (is_rec, bindings) -> List.fold_left (fun set (f, is_rec, args, typ, exp) -> BatSet.union set (get_exp_labels exp)) BatSet.empty bindings
  | _ -> BatSet.empty

let get_total_labels : prog -> label BatSet.t
= fun pgm -> List.fold_left (fun set decl -> BatSet.union set (get_decl_labels decl)) BatSet.empty pgm

let check_exp_coverage : prog -> examples -> unit
= fun pgm exs ->
	let total_labels = get_total_labels pgm in
	let executed_labels = 
		List.fold_left (fun set ex -> BatSet.union set (get_executed_labels pgm ex)) BatSet.empty exs
	  |> BatSet.filter (fun label -> BatSet.mem label total_labels) 
	in
	let uncovered_labels = BatSet.diff total_labels executed_labels in
	print_endline ("Total Exp : " ^ string_of_int (BatSet.cardinal total_labels));
	print_endline ("Covered Exp : " ^ string_of_int (BatSet.cardinal executed_labels));
	print_endline ("Uncovered Exp : " ^ string_of_int (BatSet.cardinal uncovered_labels))