open Lang
open Util
open Print
open Preprocessor
open Repair_template

module Workset = struct
  (* work = (applied, not applied) *)
  type work = (repair_templates * repair_templates)

  let rec exp_cost : lexp -> int
  = fun e -> exp_size e

  let cost_of_template : repair_template -> int
  = fun temp ->
    match temp with
    | ModifyExp (_, e) | InsertBranch (_, (_, e)) | DeleteBranch (_, (_, e)) | InsertFunction ((_, _, _, _, e), _) -> exp_cost e

  let cost : repair_templates -> int
  = fun temps -> BatSet.fold (fun temp acc -> acc + (cost_of_template temp)) temps 0 

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

  let init : repair_templates -> t
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

let rec replace_call : Type.HoleType.t -> Type.VariableType.t -> repair_template -> repair_template
= fun h_t v_t e_temp ->
  match e_temp with
  | ModifyExp (l, e) -> ModifyExp (l, replace_call_exp h_t v_t e)
  | InsertBranch (l, (p, e)) -> InsertBranch (l, (p, replace_call_exp h_t v_t e))
  | _ -> e_temp

(* Main Procedure *)
let time_out = 60.0
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

let rec get_modified_labels : prog -> repair_template -> label BatSet.t
= fun pgm temp ->
  match temp with
  | ModifyExp (l, _) -> 
    begin match get_sub pgm l with
    | None -> failwith "Fail to find modified expression"
    | Some lexp -> get_labels_exp lexp
    end
  | _ -> BatSet.empty

let rec next : prog -> Workset.t -> Workset.work -> Workset.t
= fun pgm t (a, na) -> BatSet.fold (fun temp t -> 
    let labels_applied = BatSet.fold (fun temp acc -> BatSet.union acc (get_modified_labels pgm temp)) a BatSet.empty in
    let labels_not_applied = get_modified_labels pgm temp in
    if BatSet.disjoint labels_applied labels_not_applied then
      Workset.add (BatSet.add temp a, BatSet.remove temp na) t
    else t
  ) na t 

let rec find_patch : prog -> repair_templates BatSet.t -> examples -> prog option
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
      else 
        let _ = 
          if debug_mode then 
            let score = List.fold_left (fun score (inputs, output) ->
              try
                let result_value = Eval.get_output pgm' inputs in
                if try (Eval.value_equality result_value output) with _ -> false then score+1 else score
              with 
                | EExcept v -> 
                  if try (Eval.value_equality v output) with _ -> false then score+1 else score
                | except -> score
            ) 0 testcases in
            let s = "Score : " ^ (string_of_int score) in
          print_endline s;
          Printf.fprintf (!debug) "%s\n" s
        else 
          ()
        in
        find_patch pgm remains testcases
    else find_patch pgm remains testcases

let rec extend_workset : repair_templates -> repair_templates BatSet.t -> repair_templates BatSet.t
= fun temps acc -> 
  if BatSet.is_empty temps then acc
  else if BatSet.is_empty acc then BatSet.map (fun temp -> BatSet.singleton temp) temps
  else 
    BatSet.fold (fun work_set acc ->
      let work_set = BatSet.fold (fun temp acc ->
        let new_work = BatSet.add temp work_set in
        BatSet.add new_work acc
      ) temps BatSet.empty in
      BatSet.union work_set acc
    ) acc BatSet.empty

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
      (try
        (* Replace invalid function calls in templates by speicial hole *)
        let (_, h_t, v_t, _) = Type.run (apply_templates pgm a) in
        let a = BatSet.map (fun temp -> replace_call h_t v_t temp) a in
        if debug_mode then (
          print_header "Before Updating"; 
          BatSet.iter (fun temp -> print_endline (string_of_template temp)) a;
        );
        (* Update templates having function call using call information *)
        let (_, h_t, v_t, subst) = Type.run (apply_templates pgm a) in
        let candidates = BatSet.fold (fun temp candidates -> 
          let temps = Update.update_call_templates call_temps h_t v_t subst temp in
          extend_workset temps candidates
        ) a BatSet.empty in
        (* Find patch by applying possible templates *)
        if debug_mode then (
          print_header "Updating"; 
          BatSet.iter (fun cands -> print_endline (string_of_templates cands)) candidates;
        );
        let candidates = BatSet.fold (fun a acc ->
          (* Get the type information, when all tempaltes are applied *)
          let (_, h_t, v_t, subst) = Type.run (apply_templates pgm a) in
          let candidates = BatSet.fold (fun temp candidates ->
            if debug_mode then (
              print_header "Completing before"; 
              print_endline (string_of_template temp)
            );
            let temps = Complete.complete_template h_t v_t subst temp in
            if debug_mode then (
              print_header "Completing After"; 
              print_endline (string_of_templates temps)
            );
            extend_workset temps candidates
          ) a BatSet.empty in
          BatSet.union candidates acc
        ) candidates BatSet.empty in
        if debug_mode then (
          print_header "Completing"; 
          BatSet.iter (fun cands -> print_endline (string_of_templates cands)) candidates;
        );
        begin match find_patch pgm candidates testcases with
        | None ->
          let next = next pgm remain (a, na) in
          work pgm call_temps next testcases
        | patch -> patch
        end
      with _ -> 
        let next = next pgm remain (a, na) in
        work pgm call_temps next testcases)

let run : prog -> call_templates -> repair_templates -> examples -> prog option
= fun pgm call_temps temps testcases ->
  start_time := Unix.gettimeofday();
  update_cache (program_to_string (Normalize.normalize pgm));
  let repair = work pgm call_temps (Workset.init temps) testcases in
  match repair with
  | Some pgm' -> Some pgm'
  | None -> None