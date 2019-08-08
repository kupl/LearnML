open Repairer
open Vector
open Lang

let is_fun = Cfg.S.is_fun

(*extract function*)

(*
let run : prog -> prog -> examples -> prog option
= fun pgm cpgm testcases ->
    let start_time = Unix.gettimeofday () in

    let repair_cand = 

    let repair_cand = update_var_comp pgm repair_cand in

    print_endline ("Size of repair Cand : " ^ string_of_int (BatSet.cardinal repair_cand));
    let repair = List.find_opt (fun (l,e) ->
        let pgm' = List.map(fun decl -> subst_decl decl (l,e)) pgm in
        Eval.is_solution pgm' testcases
    )  (BatSet.to_list repair_cand)

    in
    match repair with
    | Some (l,e) ->
        let pgm' = List.map (fun decl -> subst_decl decl (l,e)) pgm in
        Print.print_header "Repair result"; Print.print_pgm pgm';
        print_endline ("Time : " ^ string_of_float (Unix.gettimeofday() -. start_time));
        Some pgm'
    | None -> print_endline ("Fail to Repair"); None
*)


let extract_body : lexp -> (string*lexp) list 
= fun (l,exp) -> 
  let rec aux : exp -> (string*lexp) list
  = fun exp ->
    match exp with 
    | ELet (f, is_rec, args, typ, e1, e2) ->  
      if is_fun typ then (Print.let_to_string f,e1)::(aux e2) else aux e2
    | EFun (a,e) -> 
      let body, inner_func = aux  e in
      EFun (a, body)
  in aux exp
    
let extract_func : (string*lexp) list -> decl -> (string*lexp) list
= fun acc decl -> 
  match decl with
  | DLet (f, is_rec, args, typ, e) -> 
    let body::inner_func = extract_body e in
    if (inner_func = []) then ((Print.let_to_string f), body)::acc
    else body::inner_func::acc
  | _ -> acc 

let extract_func_all : prog -> (string*lexp) list 
= fun pgm ->
  let pgm' = Cfg.T.run pgm in
  List.fold_left (fun acc decl-> extract_func acc decl) [] pgm' 

let test : prog -> unit
= fun pgm ->
  let res = extract_func_all pgm in
  let str = List.fold_left (fun acc bind -> acc ^"\n---------------------\n" ^(Print.binding_to_string bind)) "" res in
  print_endline str 
