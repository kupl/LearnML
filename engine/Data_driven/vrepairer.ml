open Lang
open Repairer


module Workset = struct
  (* work = (applied, not applied) *)
  type work = (repair_cand BatSet.t * repair_cand BatSet.t)

  let cost : repair_cand BatSet.t -> int
  = fun set -> BatSet.fold (fun (l, e) cost -> cost + (exp_cost e)) set 0

  module OrderedType = struct
    type t = work
    let compare (a1, _) (a2, _) =
    let (c1, c2) = if (BatSet.cardinal a1) = (BatSet.cardinal a2) then (cost a1, cost a2) else (BatSet.cardinal a1, BatSet.cardinal a2) in
      if c1=c2 then 0 else
      if c1>c2 then 1
      else -1
  end

  module Heap = BatHeap.Make (OrderedType)

  (* type of workset : heap * (string set) *)
  type t = Heap.t
  let empty = Heap.empty

  let add : work -> t -> t
  = fun work t -> Heap.add work t

  let choose : t -> (work * t) option
  = fun t ->
    try
      Some (Heap.find_min t, Heap.del_min t)
    with _ -> None

  let init : repair_cand BatSet.t -> t
  = fun repair_cand -> add (BatSet.empty, repair_cand) empty

end

let time_out = 120.0
let start_time = ref 0.0

let get_repair_candidate : prog -> prog -> (string*string) list -> repair_cand BatSet.t
= fun pgm cpgm map ->
  let pgm_fmap = Extractor.extract_func_all pgm in
  let cpgm_fmap = Extractor.extract_func_all cpgm in 
  List.fold_left (fun acc (x, y) -> 
                  try 
                    let f = List.assoc x pgm_fmap in 
                    let f'= List.assoc y cpgm_fmap in
                    let repair_cand = Repairer.get_repair_candidate_exp f f' in
                    BatSet.union repair_cand acc
                  with Not_found -> acc
                 ) BatSet.empty map 

let print_mapping : (string * string) list -> unit
= fun map -> 
  print_endline ("map size " ^ string_of_int (List.length map));
  List.iter (fun (x,y) -> print_endline ("map : #" ^ x ^ " <~> #" ^ y)) map

let test : prog -> unit
= fun pgm ->
  let res = Extractor.extract_func_all pgm in
  let str = List.fold_left (fun acc (s,exp) -> acc ^ "\nFunc : " ^ s ^ "\n"^(Print.exp_to_string exp) ^"\n---------------------\n") "" res in print_endline str

let compare_exp : lexp -> lexp -> int
= fun e1 e2 ->
  let (c1, c2) = (exp_cost e1, exp_cost e2) in
  if c1 = c2 then 0 else
  if c1 > c2 then 1
  else -1

let rec next : Workset.t -> Workset.work -> Workset.t
= fun t (a, na) ->
  BatSet.fold (fun elem t -> Workset.add (BatSet.add elem a, BatSet.remove elem na) t) na t

let rec work : prog -> Workset.t -> examples -> prog option
= fun pgm workset testcases ->
  if (Unix.gettimeofday()) -. (!start_time) > time_out then None
  else 
    match Workset.choose workset with
    | None -> None
    | Some ((a, na), remain) ->
      if not (BatSet.is_empty a) then 
        let pgm' = BatSet.fold (fun cand pgm -> subst_prog pgm cand) a pgm in
        let _ = 
          Print.print_header "Patch Candidate";
          BatSet.iter (fun (l, e) -> print_endline (string_of_int l ^ " : " ^ Print.exp_to_string e)) a;
          Print.print_pgm pgm';
        in
        if Eval.is_solution pgm' testcases then
          Some pgm'
        else
          let next = next remain (a, na) in
          work pgm next testcases
      else
        let next = next remain (a, na) in
        work pgm next testcases

let run : prog -> prog -> (string*string) list -> examples -> prog option
= fun pgm cpgm map testcases ->
  start_time := Unix.gettimeofday();
  let pre_result = PreAnalysis.run cpgm in
  let repair_cand = get_repair_candidate pgm cpgm map in
  let repair_cand = gen_func_temp repair_cand in
  print_endline ("Initial Repair Cand : ");
  print_endline ("------------------------------");
  BatSet.iter (fun (l, e) -> print_endline (string_of_int l ^ " : " ^ Print.exp_to_string e)) repair_cand;
  print_endline ("------------------------------");
  let repair_cand = update_var_comp pre_result pgm repair_cand in
  let repair_cand = Function_unifier.update_func_comp pre_result pgm repair_cand in
  print_endline ("Refined Repair Cand : ");
  print_endline ("------------------------------");
  BatSet.iter (fun (l, e) -> print_endline (string_of_int l ^ " : " ^ Print.exp_to_string e)) repair_cand;
  print_endline ("------------------------------");
  print_endline ("Size of repair Cand : " ^ string_of_int (BatSet.cardinal repair_cand));
  let sorted_repair_cand = List.sort (fun (_, e1) (_, e2) -> compare_exp e1 e2) (BatSet.to_list repair_cand) in
  let repair = work pgm (Workset.init repair_cand) testcases in
  match repair with
  | Some pgm' ->
    Print.print_header "Repair result"; Print.print_pgm pgm';
    print_endline ("Time : " ^ string_of_float (Unix.gettimeofday() -. !start_time));
    Some pgm'
  | None -> print_endline ("Fail to Repair"); None