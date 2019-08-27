open Lang

type t = (string * lexp) list

let is_fun = Cfg.S.is_fun
let get_bindvar = Print.let_to_string 
    
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
    if args <> [] || is_fun typ then 
      let (e1',env') = extract_body env e1 in
      let (e2',env'') = extract_body env' e2 in
      e2', ((get_bindvar f),e1')::env'' 
    else 
      let (body,env') = extract_body env e2 in
      (l,ELet (f, is_rec,args, typ, e1, body)), env'
  | EFun (a,e) -> extract_body env e 
  (*EMatch, EBlock, *)
  | EMatch (e,bs) -> 
    let rec flatten_branch : branch list -> branch list
    = fun bs ->
      match bs with 
      | [] -> []
      | (p,e)::bs ->
        begin match p with
        | Pats ps -> let flat_bs = (List.map (fun p -> (p,e)) ps) in
          (flatten_branch flat_bs) @ (flatten_branch bs)
        | _ -> (p,e) :: (flatten_branch bs)
        end
    in 
    let bs = flatten_branch bs in
    let (e',env') = extract_body env e in
    let (bs',env'') = 
      List.fold_left (fun (bs, cur_env) (p, cur_e) ->
        let (ne,nenv) = extract_body cur_env cur_e in
        ((p,ne)::bs, nenv) 
      ) ([], env') bs
    in (l, EMatch (e',bs')), env''
  | _ -> (l,exp), env

    
let rec extract_func : t -> decl -> t
= fun acc decl -> 
  match decl with
  | DLet (f, is_rec, args, typ, e) -> 
    let empty = [] in 
    let body, inner_func= extract_body empty e in
   ((get_bindvar f, body)::inner_func)@acc
  | DBlock (is_rec, bindings) -> List.fold_left (fun acc binding -> extract_func acc (DLet binding)) acc bindings
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
