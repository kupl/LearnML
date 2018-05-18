open Lang
open Util
open Print

type count = int

let float : int -> float = float_of_int

let int : float -> int = int_of_float

(* Preprocessing for the function entry *)

let rec extract_args : lexp -> label BatSet.t
= fun (l, e) ->
	match e with
	| EFun (arg, e) -> BatSet.add l (extract_args e)
	|_ -> BatSet.empty
	

let preprocess_decl : decl -> label BatSet.t -> label BatSet.t
= fun decl label_set ->
	match decl with
	| DLet (f, is_rec, args, typ, e) ->
		BatSet.union (extract_args e) label_set
	| DBlock (is_rec, bindings) ->
		list_fold (fun (_, _, _, _, e) acc -> BatSet.union (extract_args e) acc) bindings label_set
	|_  -> label_set

let preprocess : prog -> label BatSet.t
= fun pgm -> list_fold preprocess_decl pgm BatSet.empty


(* Find counter exampels *)

let run_pgm : prog -> example -> env
= fun pgm (input, output) ->
  let res_var = "__res__" in
  let pgm = pgm@(External.grading_prog) in
  let pgm' = pgm @ [(DLet (BindOne res_var,false,[],fresh_tvar(),(Lang.appify (gen_label(),EVar !Options.opt_entry_func) input)))] in
  Eval.run pgm'

let rec is_counter_example : prog -> example -> bool
= fun pgm (input, output) ->
  try
    let env = run_pgm pgm (input,output) in
    let result = Lang.lookup_env "__res__" env in
    not (Eval.value_equality result output)
  with e -> print_endline(Printexc.to_string e);true

let rec find_counter_examples : prog -> examples -> examples * examples
= fun pgm examples -> List.partition (is_counter_example pgm) examples

(*****************************************************************)
(*      generate holed program with suspicious expression        *)
(*****************************************************************)

let rec gen_partial_exp : label -> lexp -> lexp
= fun label (l,exp) ->
  if l=label then (l,gen_hole())
  else 
    let exp = 
      begin match exp with
      | NOT e -> NOT (gen_partial_exp label e)
      | MINUS e -> MINUS (gen_partial_exp label e)
      | EFun (arg, e) -> EFun (arg, gen_partial_exp label e)
      | Raise e -> Raise (gen_partial_exp label e)
      | ADD (e1, e2) -> ADD (gen_partial_exp label e1, gen_partial_exp label e2)
      | SUB (e1, e2) -> SUB (gen_partial_exp label e1, gen_partial_exp label e2)
      | MUL (e1, e2) -> MUL (gen_partial_exp label e1, gen_partial_exp label e2)
      | DIV (e1, e2) -> DIV (gen_partial_exp label e1, gen_partial_exp label e2)
      | MOD (e1, e2) -> MOD (gen_partial_exp label e1, gen_partial_exp label e2)
      | OR (e1, e2) -> OR (gen_partial_exp label e1, gen_partial_exp label e2)
      | AND (e1, e2) -> AND (gen_partial_exp label e1, gen_partial_exp label e2)
      | LESS (e1, e2) -> LESS (gen_partial_exp label e1, gen_partial_exp label e2)
      | LARGER (e1, e2) -> LARGER (gen_partial_exp label e1, gen_partial_exp label e2)
      | EQUAL (e1, e2) -> EQUAL (gen_partial_exp label e1, gen_partial_exp label e2)
      | NOTEQ (e1, e2) -> NOTEQ (gen_partial_exp label e1, gen_partial_exp label e2)
      | LESSEQ (e1, e2) -> LESSEQ (gen_partial_exp label e1, gen_partial_exp label e2)
      | LARGEREQ (e1, e2) -> LARGEREQ (gen_partial_exp label e1, gen_partial_exp label e2)
      | AT (e1, e2) -> AT (gen_partial_exp label e1, gen_partial_exp label e2)
      | DOUBLECOLON (e1, e2) -> DOUBLECOLON (gen_partial_exp label e1, gen_partial_exp label e2)
      | STRCON (e1, e2) -> STRCON (gen_partial_exp label e1, gen_partial_exp label e2)
      | EApp (e1, e2) -> EApp (gen_partial_exp label e1, gen_partial_exp label e2)
      | ELet (f, is_rec, args, t, e1, e2) -> ELet (f, is_rec, args, t, gen_partial_exp label e1, gen_partial_exp label e2)
      | IF (e1, e2, e3) -> IF (gen_partial_exp label e1, gen_partial_exp label e2, gen_partial_exp label e3)
      | ECtor (x, elst) -> ECtor (x, list_map (gen_partial_exp label) elst)
      | EList elst -> EList (list_map (gen_partial_exp label) elst)
      | ETuple elst -> ETuple (list_map (gen_partial_exp label) elst)
      | EBlock (is_rec, bindings, e) ->
        let bindings = list_map (fun (f, is_rec, args, t, e) -> (f, is_rec, args, t, gen_partial_exp label e)) bindings in
        EBlock (is_rec, bindings, gen_partial_exp label e)
      | EMatch (e, blst) -> EMatch (gen_partial_exp label e, list_map (fun (p, e) -> (p,gen_partial_exp label e)) blst)
      |_ -> exp
      end in
    (l,exp) 

let gen_partial_decl : label -> decl -> decl
= fun label decl ->
  match decl with
  | DLet (f, is_rec, args, t, e) -> DLet (f, is_rec, args, t, gen_partial_exp label e)
  | DBlock (is_rec, bindings) ->
    let bindings = list_map (fun (f, is_rec, args, t, e) -> (f, is_rec, args, t, gen_partial_exp label e)) bindings in
    DBlock (is_rec, bindings) 
  |_ -> decl

let gen_partial_pgm : label -> prog -> prog
= fun label pgm -> list_map (fun decl -> gen_partial_decl label decl) pgm


(**********************************************************************)
(* gathering the trace information about testcase and calculate score *)
(**********************************************************************)

let rec trace_info : prog -> examples -> (label, count) BatMap.t
= fun pgm examples ->
  list_fold(fun example map ->
  	let _ = try run_pgm pgm example with |_ -> empty_env in
    let trace = list2set (!Eval.trace_set) in
    BatSet.fold(fun label map ->
      let count = BatMap.find_default 0 label map in
      BatMap.add label (count+1) map
    ) trace map
  ) examples BatMap.empty

let localization : prog -> examples -> (int * prog) BatSet.t
= fun pgm examples ->
	let dummy_labels = preprocess pgm in
	let (neg,pos) = find_counter_examples pgm examples in
  Eval.trace_option := true;
  let neg_map = trace_info pgm neg in
  let pos_map = trace_info pgm pos in
  Eval.trace_option := false;
	let neg_map = BatMap.filter (fun label _ -> not(BatSet.mem label dummy_labels)) neg_map in
  let cand_pgm_set = BatMap.foldi (fun label _ set -> 
    let cand_pgm = gen_partial_pgm label pgm in
    if (pgm = cand_pgm) then set
    else
      let size = cost pgm - cost cand_pgm + exp_cost(0,Hole(0)) in 
      BatSet.add (label, cand_pgm,size) set
  ) neg_map BatSet.empty in
  let cand_count = BatSet.cardinal cand_pgm_set in
  let weight_sum = BatSet.fold (fun (_, _, size) sum -> sum + size) cand_pgm_set 0 in
  let average = weight_sum / cand_count in
  BatSet.map (fun (label, cand_pgm, size) ->
    let pos_count = (BatMap.find_default 0 label pos_map) in
		(*let neg_count = (BatMap.find_default 0 label neg_map) in
		let total = neg_count + pos_count in*)
		let total = List.length examples in
    let score = size + int((float(pos_count) /. float(total)) *. (float(average))) in
    (score,cand_pgm)
  ) cand_pgm_set
