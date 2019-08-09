open Repairer
open Vector
open Lang

let is_fun = Cfg.S.is_fun
let get_bindvar = Print.let_to_string 

type t = (string * lexp) list
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


let rec extract_body : t -> lexp -> lexp * t 
= fun env (l,exp) -> 
  match exp with 
  | ADD (e1,e2) -> 
    let (e1', env') = extract_body env e1 in
    let (e2',env'') = extract_body env' e2 in
    (l,ADD (e1',e2')), env''
  | SUB (e1,e2) ->
    let (e1', env') = extract_body env e1 in
    let (e2', env'') = extract_body env' e2 in
    (l,SUB (e1',e2')), env''
  | IF (e1,e2,e3) ->
    let (e1',env1) = extract_body env e1 in
    let (e2',env2) = extract_body env1 e2 in
    let (e3',env3) = extract_body env2 e3 in
    (l,IF (e1',e2',e3')), env3 
  | ELet (f, is_rec, args, typ, e1, e2) ->
    if is_fun typ then 
      let (e1',env') = extract_body env e1 in
      let (e2',env'') = extract_body env' e2 in
      e2', ((Print.let_to_string f),e1')::env'' 
    else 
      let (body,env') = extract_body env e2 in
      (l,ELet (f, is_rec,args, typ, e1, body)), env'
  | EFun (a,e) -> extract_body env e 
  | _ -> (l,exp), env
    
let extract_func : t -> decl -> t
= fun acc decl -> 
  match decl with
  | DLet (f, is_rec, args, typ, e) -> 
    let empty = [] in 
    let body, inner_func= extract_body empty e in
    acc@((Print.let_to_string f, body)::inner_func)
  | _ -> acc 

let extract_func_all : prog -> t
= fun pgm ->
  let pgm' = Cfg.T.run pgm in
  List.fold_left (fun acc decl-> extract_func acc decl) [] pgm' 

let test : prog -> unit
= fun pgm ->
  let res = extract_func_all pgm in
  let str = List.fold_left (fun acc (s, exp) -> acc ^"\n---------------------\n" ^ "Func : " ^ s ^"\n"^(Print.exp_to_string exp)) "" res in
  print_endline str 
