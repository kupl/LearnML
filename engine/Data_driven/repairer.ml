open Lang
open Util
open Extractor
open Complete

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

  let cost : repair_template BatSet.t -> int
  = fun set -> BatSet.fold (fun (l, e) cost -> cost + (exp_cost e)) set 0

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

let time_out = 60.0
let start_time = ref 0.0

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
        let pgm' = BatSet.fold (fun cand pgm -> subst_pgm pgm cand) a pgm in
        (*
        let _ = 
          Print.print_header "Patch Candidate";
          BatSet.iter (fun (l, e) -> print_endline (string_of_int l ^ " : " ^ Print.exp_to_string e)) a;
          Print.print_pgm pgm';
        in
    	*)
        if Eval.is_solution pgm' testcases then
          Some pgm'
        else
          let next = next remain (a, na) in
          work pgm next testcases
      else
        let next = next remain (a, na) in
        work pgm next testcases

let run : prog -> repair_template BatSet.t -> examples -> prog option
= fun pgm temps testcases ->
  start_time := Unix.gettimeofday();
  let repair = work pgm (Workset.init temps) testcases in
  match repair with
  | Some pgm' -> Some pgm'
  | None -> print_endline ("Fail to Repair"); None