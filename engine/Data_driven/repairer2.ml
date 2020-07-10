open Lang
open Util
open Print
open Repair_template
open Extractor2

module Workset = struct
  (* work = (applied, not applied) *)
  type work = (repair_template BatSet.t * repair_template BatSet.t)

  let rec exp_cost : lexp -> int
  = fun (_, exp) ->
    match exp with
    | MINUS e | NOT e | EFun (_, e) | Raise e -> 1 + exp_cost e
    | EList es | ECtor (_, es) | ETuple es -> List.fold_left (fun acc e -> exp_cost e + acc) 1 es
    | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2)
    | OR (e1, e2) | AND (e1, e2) | EQUAL (e1, e2) | NOTEQ (e1, e2)
    | LESS (e1, e2) | LARGER (e1, e2)| LESSEQ (e1, e2) | LARGEREQ (e1, e2)
    | AT (e1, e2) | DOUBLECOLON (e1, e2) | STRCON (e1, e2) | EApp (e1, e2) 
    | ELet (_, _, _, _, e1, e2) -> 1 + (exp_cost e1) + (exp_cost e2)
    | IF (e1, e2, e3) -> 1 + (exp_cost e1) + (exp_cost e2) + (exp_cost e3)
    | EMatch (e, bs) -> 
      let es = e::(List.map (fun (p, e) -> e) bs) in
      List.fold_left (fun acc e -> exp_cost e + acc) 1 es
    | EBlock (_, bs, e) -> 
      let es = e::(List.map (fun (_, _, _, _, e) -> e) bs) in
      List.fold_left (fun acc e -> exp_cost e + acc) 1 es
    | _ -> 1

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

  type t = Heap.t
  let empty = Heap.empty

  let add : work -> t -> t
  = fun work t -> Heap.add work t

  let choose : t -> (work * t) option
  = fun t ->
    try
      Some (Heap.find_min t, Heap.del_min t)
    with _ -> None

  let init : repair_template BatSet.t -> t
  = fun temps -> add (BatSet.empty, temps) empty
end

(* Pruning *)
let rec get_exp_exp : lexp -> label -> lexp option
= fun (l', exp) l ->
  if l = l' then Some (l', exp) else 
  match exp with
  | EList es | ECtor (_, es) | ETuple es -> get_exp_list es l
  | EFun (_, e) | MINUS e | NOT e -> get_exp_exp e l
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) 
  | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LARGER (e1, e2) | LESSEQ (e1, e2) | LARGEREQ (e1, e2) 
  | EQUAL (e1, e2) | NOTEQ (e1, e2) | AT (e1, e2) | DOUBLECOLON (e1, e2) | STRCON (e1, e2) | EApp (e1, e2) 
  | ELet (_, _, _, _, e1, e2) -> get_exp_list [e1; e2] l
  | EBlock (_, ds, e) ->
    let es = e::(List.map (fun (_, _, _, _, e) -> e) ds) in
    get_exp_list es l
  | EMatch (e, bs) ->
    let es = e::(List.map (fun (p, e) -> e) bs) in
    get_exp_list es l
  | IF (e1, e2, e3) -> get_exp_list [e1; e2; e3] l
  | _ -> None

and get_exp_list : lexp list -> label -> lexp option
= fun es l ->
  match es with
  | [] -> None
  | hd::tl ->   
    begin match get_exp_exp hd l with
    | Some e' -> Some e'
    | None -> get_exp_list tl l
    end

let rec get_exp_decl : decl -> label -> lexp option
= fun decl l -> 
  match decl with
  | DLet (_, _, _, _, e) -> get_exp_exp e l 
  | DBlock (_, ds) -> List.fold_left (fun acc (_, _, _, _, e) -> if acc = None then get_exp_exp e l else acc) None ds
  | _ -> None

let rec get_exp : prog -> label -> lexp option
= fun pgm l -> List.fold_left (fun acc decl -> if acc = None then get_exp_decl decl l else acc) None pgm

let rec remove_redundant : prog -> exp_template BatSet.t -> exp_template BatSet.t
= fun pgm e_temps ->
  (* If modification does not change original expression, prune out *)
  BatSet.filter (fun e_temp ->
    match e_temp with
    | ModifyExp (l, e) -> 
      begin match get_exp pgm l with
      | Some e' -> (Print.exp_to_string e) <> (Print.exp_to_string e')
      | None -> true
      end
    | _ -> true 
  ) e_temps 

let rec check_redundant : prog -> exp_template -> bool
= fun pgm e_temp ->
  (* If modification does not change one of the tempaltes, resulting candidates must be redundant *)
  match e_temp with
  | ModifyExp (l, e) -> 
    begin match get_exp pgm l with
    | Some e' -> (Print.exp_to_string e) = (Print.exp_to_string e')
    | None -> false
    end
  | _ -> false

(* Main Procedure *)
let time_out = 600.0
let start_time = ref 0.0
let debug = ref (open_out "log.txt")

let rec next : Workset.t -> Workset.work -> Workset.t
= fun t (a, na) ->
  BatSet.fold (fun elem t -> Workset.add (BatSet.add elem a, BatSet.remove elem na) t) na t

let rec find_patch : prog -> (repair_template BatSet.t) BatSet.t -> examples -> prog option
= fun pgm candidates testcases ->
  if BatSet.is_empty candidates then None
  else 
    let (cand, remains) = BatSet.pop candidates in
    let pgm' = apply_templates pgm cand in  
    if (Infinite.Static.run pgm') || (Print.program_to_string pgm = Print.program_to_string pgm') && false then
      (* Simple pruning *)
      find_patch pgm remains testcases
    else
      let _ = 
        let header = "------------------------\nPatch Candidate\n------------------------\n" in
        Printf.fprintf (!debug) "%s\n" (header ^ (string_of_templates cand) ^ "\n");
        let result = "------------------------\nResult\n------------------------\n" in
        Printf.fprintf (!debug) "%s\n" (result ^ (Print.program_to_string pgm') ^ "\n")
        (*
        Print.print_header "Patch Candidate"; print_endline (string_of_templates cand);
        Print.print_header "Result"; Print.print_pgm pgm'
        *)
      in
      if Eval.is_solution pgm' testcases then Some pgm' else find_patch pgm remains testcases

(* Refactoring is needed *)
let rec work : prog -> Workset.t -> examples -> prog option
= fun pgm workset testcases ->
  if (Unix.gettimeofday()) -. (!start_time) > time_out then None
  else 
    match Workset.choose workset with
    | None -> None
    | Some ((a, na), remain) ->
      (*
      let _ =
        print_header "Apply"; print_endline (string_of_templates a);
        print_header "Not-Apply"; print_endline (string_of_templates na)
      in
      *)
      if not (BatSet.is_empty a) then 
        (try
          (* Get the type information, when all tempaltes are applied *)
          let pgm' = apply_templates pgm a in
          let (_, h_t, v_t, subst) = Type.run pgm' in
          (*
          let _ = 
            print_header "Pgm'"; Print.print_pgm2 pgm';
            print_header "HT";
            Type.HoleType.print h_t;
            print_header "VT";
            Type.VariableType.print v_t;
            print_header "SB";
            Type.Subst.print subst
          in
          *)
          (* Based on the type information complete each template *)
          let candidates = BatSet.fold (fun (e_temp, d_temp) candidates -> 
            let e_temps = Complete2.complete_template h_t v_t subst e_temp in
            (*
            let _ = 
              print_header "Complete result";
              print_endline (string_of_set (string_of_exp_template) e_temps)
            in
            *)
            (* let e_temps = remove_redundant pgm e_temps in *)
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
          let candidates = BatSet.filter (fun cand -> 
            BatSet.for_all (fun (e_temp, d_temp) -> not (check_redundant pgm e_temp)) cand
          ) candidates in
          begin match find_patch pgm candidates testcases with
          | None ->
            let next = next remain (a, na) in
            work pgm next testcases
          | patch -> patch
          end
        with Type.TypeError -> 
          let next = next remain (a, na) in
          work pgm next testcases)
      else
        let next = next remain (a, na) in
        work pgm next testcases

let run : prog -> repair_template BatSet.t -> examples -> prog option
= fun pgm temps testcases ->
  start_time := Unix.gettimeofday();
  let repair = work pgm (Workset.init temps) testcases in
  match repair with
  | Some pgm' -> Some pgm'
  | None -> None