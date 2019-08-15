open Repairer
open Lang

let is_fun = Cfg.S.is_fun
let get_bindvar = Print.let_to_string 

type t = (string * lexp) list
    
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
  | MUL (e1,e2) ->
    let (e1', env') = extract_body env e1 in
    let (e2', env'') = extract_body env' e2 in
    (l,MUL (e1',e2')), env''
  | DIV (e1,e2) ->
    let (e1', env') = extract_body env e1 in
    let (e2', env'') = extract_body env' e2 in
    (l,DIV (e1',e2')), env''
  | MOD (e1,e2) ->
    let (e1', env') = extract_body env e1 in
    let (e2', env'') = extract_body env' e2 in
    (l,MOD (e1',e2')), env''
  | OR (e1,e2) ->
    let (e1', env') = extract_body env e1 in
    let (e2', env'') = extract_body env' e2 in
    (l,OR (e1',e2')), env''
  | AND (e1,e2) ->
    let (e1', env') = extract_body env e1 in
    let (e2', env'') = extract_body env' e2 in
    (l,AND (e1',e2')), env''
  | LESS (e1,e2) ->
    let (e1', env') = extract_body env e1 in
    let (e2', env'') = extract_body env' e2 in
    (l,LESS (e1',e2')), env''
  | LARGER (e1,e2) ->
    let (e1', env') = extract_body env e1 in
    let (e2', env'') = extract_body env' e2 in
    (l,LARGER (e1',e2')), env''
  | EQUAL (e1,e2) ->
    let (e1', env') = extract_body env e1 in
    let (e2', env'') = extract_body env' e2 in
    (l,EQUAL (e1',e2')), env''
  | NOTEQ (e1,e2) ->
    let (e1', env') = extract_body env e1 in
    let (e2', env'') = extract_body env' e2 in
    (l,NOTEQ (e1',e2')), env''
  | LESSEQ (e1,e2) ->
    let (e1', env') = extract_body env e1 in
    let (e2', env'') = extract_body env' e2 in
    (l,LESSEQ (e1',e2')), env''
  | LARGEREQ (e1,e2) ->
    let (e1', env') = extract_body env e1 in
    let (e2', env'') = extract_body env' e2 in
    (l,LARGEREQ (e1',e2')), env''
  | AT (e1,e2) ->
    let (e1', env') = extract_body env e1 in
    let (e2', env'') = extract_body env' e2 in
    (l,AT (e1',e2')), env''
  | DOUBLECOLON (e1,e2) ->
    let (e1', env') = extract_body env e1 in
    let (e2', env'') = extract_body env' e2 in
    (l,DOUBLECOLON (e1',e2')), env''
  | STRCON (e1,e2) ->
    let (e1', env') = extract_body env e1 in
    let (e2', env'') = extract_body env' e2 in
    (l,STRCON (e1',e2')), env''
  | IF (e1,e2,e3) ->
    let (e1',env1) = extract_body env e1 in
    let (e2',env2) = extract_body env1 e2 in
    let (e3',env3) = extract_body env2 e3 in
    (l,IF (e1',e2',e3')), env3 
  | EApp (e1,e2) ->
    let (e1', env') = extract_body env e1 in
    let (e2', env'') = extract_body env' e2 in
    (l,EApp (e1',e2')), env''
  | ELet (f, is_rec, args, typ, e1, e2) ->
    if is_fun typ then 
      let (e1',env') = extract_body env e1 in
      let (e2',env'') = extract_body env' e2 in
      e2', ((get_bindvar f),e1')::env'' 
    else 
      let (body,env') = extract_body env e2 in
      (l,ELet (f, is_rec,args, typ, e1, body)), env'
  | EFun (a,e) -> extract_body env e 
  (*EMatch, EBlock, *)
  | EMatch (e,bs) -> (l,exp), env
  | _ -> (l,exp), env
    
let extract_func : t -> decl -> t
= fun acc decl -> 
  match decl with
  | DLet (f, is_rec, args, typ, e) -> 
    let empty = [] in 
    let body, inner_func= extract_body empty e in
    acc@((get_bindvar f, body)::inner_func)
  | _ -> acc 

let extract_func_all : prog -> t
= fun pgm ->
  let pgm' = Cfg.T.run pgm in
  List.fold_left (fun acc decl-> extract_func acc decl) [] pgm' 

let test : t -> unit
= fun res ->
  let str = List.fold_left (fun acc (s, exp) -> acc ^"\n---------------------\n"
            ^ "Func : " ^ s ^"\n"^(Print.exp_to_string exp)) "" res in
  print_endline str 

let get_repair_candidate : prog -> prog -> unit
= fun pgm cpgm ->
  let pgm_fmap = extract_func_all pgm in
  let cpgm_fmap = extract_func_all cpgm in 
  test pgm_fmap; test cpgm_fmap 
(*
let run : prog -> prog -> examples -> prog option
= fun pgm cpgm testcases ->
    let start_time = Unix.gettimeofday () in
    let repair_cand = get_repair_candidate pgm cpgm in
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
