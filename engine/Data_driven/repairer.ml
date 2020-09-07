open Lang
open Util
open Print
open Preprocessor
open Repair_template

module Workset = struct
  (* work = (applied, not applied) *)
  type work = (repair_template BatSet.t * repair_template BatSet.t)

  let rec exp_cost : lexp -> int
  = fun e -> exp_size e

  let e_temp_cost : exp_template -> int
  = fun e_temp ->
    match e_temp with
    | ModifyExp (_, e) | InsertBranch (_, (_, e)) -> exp_cost e
    | DeleteBranch (_, (_, e)) -> exp_cost e

  let cost : repair_template BatSet.t -> int
  = fun temp -> 
    let (e_temps, d_temps) = merge_templates temp in
    BatSet.fold (fun e_temp acc -> acc + (e_temp_cost e_temp)) e_temps 0 + BatMap.cardinal d_temps

  module OrderedType = struct
    type t = work
    let compare (a1, _) (a2, _) =
    let (c1, c2) = if BatSet.cardinal a1 = BatSet.cardinal a2 then (cost a1, cost a2) else (BatSet.cardinal a1, BatSet.cardinal a2) in
      if c1=c2 then 0 else
      if c1>c2 then 1
      else -1
  end

  module Heap = BatHeap.Make (OrderedType)

  (* t = workset * explored *)
  type t = Heap.t * work BatSet.t
  let empty = (Heap.empty, BatSet.empty)

  let explored : work -> t -> bool
  = fun work (_, prev) -> BatSet.mem work prev 
    
  let add : work -> t -> t
  = fun work (heap, prev) -> 
    if explored work (heap, prev) then (heap, prev) 
    else (Heap.add work heap, BatSet.add work prev)

  let choose : t -> (work * t) option
  = fun (heap, prev) -> 
    try
      let elem = Heap.find_min heap in
      Some (elem, (Heap.del_min heap, prev)) 
    with _ -> None

  let init : repair_template BatSet.t -> t
  = fun temps -> add (BatSet.empty, temps) empty

  let print : t -> unit
  = fun (heap, prev) ->
    List.iter (fun (a, na) ->
      print_endline ("Apply : " ^ string_of_templates a);
      print_endline ("Not-Applied : " ^ string_of_templates na)
    ) (Heap.to_list heap)
end

(* Check the template has invalid function call *)
let rec is_valid_call : Type.HoleType.t -> Type.VariableType.t -> lexp -> bool
= fun h_t v_t (l, exp) ->
  match exp with
  | EApp (e1, e2) -> is_valid_call h_t v_t e1
  | Hole n -> 
    let hole_typ = BatMap.find n h_t in
    BatMap.exists (fun x typ -> Type.check_typs hole_typ typ) (BatMap.find n v_t)
  | _ -> true

let rec replace_call_exp : Type.HoleType.t -> Type.VariableType.t -> lexp -> lexp
= fun h_t v_t (l, exp) ->
  match exp with
  | EApp (e1, e2) ->  
    if is_valid_call h_t v_t e1 then (l, EApp (replace_call_exp h_t v_t e1, replace_call_exp h_t v_t e2))
    else (l, gen_fhole ())
  | EFun (arg, e) -> (l, EFun (arg, replace_call_exp h_t v_t e))
  | ERef e | EDref e | Raise e | MINUS e | NOT e -> (l, update_unary exp (replace_call_exp h_t v_t e))
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) 
  | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LESSEQ (e1, e2) | LARGER (e1, e2) | LARGEREQ (e1, e2) 
  | EQUAL (e1, e2) | NOTEQ (e1, e2) | DOUBLECOLON (e1, e2) | AT (e1, e2) | STRCON (e1, e2) 
  | EAssign (e1, e2) -> (l, update_binary exp (replace_call_exp h_t v_t e1, replace_call_exp h_t v_t e2))
  | EList es -> (l, EList (List.map (fun e -> replace_call_exp h_t v_t e) es))
  | ETuple es -> (l, ETuple (List.map (fun e -> replace_call_exp h_t v_t e) es))
  | ECtor (x, es) -> (l, ECtor (x, List.map (fun e -> replace_call_exp h_t v_t e) es))
  | IF (e1, e2, e3) -> (l, IF (replace_call_exp h_t v_t e1, replace_call_exp h_t v_t e2, replace_call_exp h_t v_t e3))
  | EMatch (e, bs) -> 
    let bs = List.map (fun (p, e) -> (p, replace_call_exp h_t v_t e)) bs in
    (l, EMatch (replace_call_exp h_t v_t e, bs))
  | ELet (f, is_rec, args, typ, e1, e2) -> (l, ELet (f, is_rec, args, typ, replace_call_exp h_t v_t e1, replace_call_exp h_t v_t e2))
  | EBlock (is_rec, bindings, e2) -> 
    let ds = List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, replace_call_exp h_t v_t e)) bindings in
    (l, EBlock (is_rec, ds, replace_call_exp h_t v_t e2))
  | _ -> (l, exp)

let rec replace_call : Type.HoleType.t -> Type.VariableType.t -> exp_template -> exp_template
= fun h_t v_t e_temp ->
  match e_temp with
  | ModifyExp (l, e) -> ModifyExp (l, replace_call_exp h_t v_t e)
  | InsertBranch (l, (p, e)) -> InsertBranch (l, (p, replace_call_exp h_t v_t e))
  | _ -> e_temp

(* Main Procedure *)
let time_out = 600.0
let start_time = ref 0.0
let debug_mode = false

let debug = ref (open_out "log.txt")
let cache = ref BatSet.empty (* Cacheing for stroing redundant candidate programs *)

let update_cache s_pgm = cache := BatSet.add s_pgm (!cache)

let check_cache s_pgm = 
  if BatSet.mem s_pgm (!cache) then true 
  else 
    let _ = update_cache s_pgm in 
    false

let rec next : Workset.t -> Workset.work -> Workset.t
= fun t (a, na) ->
  (*
  let _ =
    print_header "Before"; Workset.print t
    print_header "Current Apply"; print_endline (string_of_templates a);
    print_header "Current Not-Apply"; print_endline (string_of_templates na)
  in
  *)
  let t = BatSet.fold (fun elem t -> 
    Workset.add (BatSet.add elem a, BatSet.remove elem na) t
  ) na t in
  (*
  let _ =
    print_header "After"; Workset.print t
  in
  *)
  t

let rec find_patch : prog -> (repair_template BatSet.t) BatSet.t -> examples -> prog option
= fun pgm candidates testcases ->
  if BatSet.is_empty candidates then None
  else 
    let (cand, remains) = BatSet.pop candidates in
    let pgm' = apply_templates pgm cand in  
    let s_pgm = program_to_string (Normalize.normalize pgm') in
    if not (check_cache s_pgm) then
      let _ = 
        if debug_mode then 
          let s = 
            "------------------------\nPatch Candidate\n------------------------\n" ^
            string_of_templates cand ^ "\n" ^
            "------------------------\nResult\n------------------------\n" ^
            Print.program_to_string pgm' ^ "\n"
          in
          print_endline s;
          Printf.fprintf (!debug) "%s\n" s
        else 
          ()
      in
      if Infinite.Static.run pgm' then find_patch pgm remains testcases
      else if Eval.is_solution pgm' testcases then Some pgm' 
      else find_patch pgm remains testcases
    else find_patch pgm remains testcases


(* If two templates fix intersect point simultaneously they are incompatible => *)
let check_intersect : prog -> repair_template BatSet.t -> bool
= fun pgm templates ->
  BatSet.exists (fun (e_temp, d_temp) ->
    match e_temp with
    | ModifyExp (l, _) -> 
      let labels1 = 
        match get_sub pgm l with
        | None -> failwith "Fail to find subexpression"
        | Some lexp -> get_labels_exp lexp
      in 
      BatSet.exists (fun (e_temp, d_temp) ->
        match e_temp with
        | ModifyExp (l', _) -> 
          let labels2 = 
             match get_sub pgm l' with
            | None -> failwith "Fail to find subexpression"
            | Some lexp -> get_labels_exp lexp
          in 
          not (BatSet.disjoint labels1 labels2)
        | _ -> false 
      ) (BatSet.remove (e_temp, d_temp) templates)
    | _ -> false 
  ) templates

let rec work : prog -> call_templates -> Workset.t -> examples -> prog option
= fun pgm call_temps workset testcases ->
  if (Unix.gettimeofday()) -. (!start_time) > time_out then None
  else 
    match Workset.choose workset with
    | None -> None
    | Some ((a, na), remain) ->
      if debug_mode then (
        print_header "Apply"; print_endline (string_of_templates a);
        print_header "Not-Apply"; print_endline (string_of_templates na)
      );
      if check_intersect pgm a then
        work pgm call_temps remain testcases
      else (try
        (* Replace invalid function calls in templates by speicial hole *)
        let (_, h_t, v_t, _) = Type.run (apply_templates pgm a) in
        let a = BatSet.map (fun (e_temp, d_temp) -> (replace_call h_t v_t e_temp, d_temp)) a in
        if debug_mode then (
          print_header "Before Updating"; 
          BatSet.iter (fun temp -> print_endline (string_of_template temp)) a;
        );
        (* Update templates having function call using call information *)
        let (_, h_t, v_t, subst) = Type.run (apply_templates pgm a) in
        let candidates = BatSet.fold (fun (e_temp, d_temp) candidates -> 
          let e_temps = Update.update_call_templates call_temps h_t v_t subst e_temp in
          if BatSet.is_empty e_temps then candidates
          else if BatSet.is_empty candidates then
            BatSet.map (fun e_temp -> BatSet.singleton (e_temp, d_temp)) e_temps
          else
            BatSet.fold (fun cand acc ->
              let candidates = BatSet.fold (fun e_temp candidates ->
                let cand = BatSet.add (e_temp, d_temp) cand in
                BatSet.add cand candidates
              ) e_temps BatSet.empty in 
              BatSet.union candidates acc
            ) candidates BatSet.empty
        ) a BatSet.empty in
        (* Find patch by applying possible templates *)
        if debug_mode then (
          print_header "Updating"; 
          BatSet.iter (fun cands -> print_endline (string_of_templates cands)) candidates;
        );
        let candidates = BatSet.fold (fun a acc ->
          (* Get the type information, when all tempaltes are applied *)
          let pgm' = apply_templates pgm a in
          let (_, h_t, v_t, subst) = Type.run pgm' in
          let candidates = BatSet.fold (fun (e_temp, d_temp) candidates ->
            let e_temps = Complete2.complete_template call_temps h_t v_t subst e_temp in
            if BatSet.is_empty candidates then
              BatSet.map (fun e_temp -> BatSet.singleton (e_temp, d_temp)) e_temps
            else
              BatSet.fold (fun cand acc ->
                let candidates = BatSet.fold (fun e_temp candidates ->
                  let cand = BatSet.add (e_temp, d_temp) cand in
                  BatSet.add cand candidates
                ) e_temps BatSet.empty in 
                BatSet.union candidates acc
              ) candidates BatSet.empty
          ) a BatSet.empty in
          BatSet.union candidates acc
        ) candidates BatSet.empty in
        if debug_mode then (
          print_header "Completing"; 
          BatSet.iter (fun cands -> print_endline (string_of_templates cands)) candidates;
        );
        begin match find_patch pgm candidates testcases with
        | None ->
          let next = next remain (a, na) in
          work pgm call_temps next testcases
        | patch -> patch
        end
      with Type.TypeError -> 
        let next = next remain (a, na) in
        work pgm call_temps next testcases)

let run : prog -> call_templates -> repair_template BatSet.t -> examples -> prog option
= fun pgm call_temps temps testcases ->
  start_time := Unix.gettimeofday();
  update_cache (program_to_string (Normalize.normalize pgm));
  let repair = work pgm call_temps (Workset.init temps) testcases in
  match repair with
  | Some pgm' -> Some pgm'
  | None -> None