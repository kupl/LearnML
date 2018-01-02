open Lang
open Util

let rec types_to_arr (ts:typ list) =
  match ts with
  | []  -> raise (Failure "(types_to_arr) empty type list provided")
  | [t] -> t
  | t :: ts -> TArr (t, types_to_arr ts)

let rec find_first_branch : value -> branch list -> branch 
= fun v bs ->
  match bs with  
  | [] -> raise (Failure "Pattern matching failure")
  | (p, ex)::tl ->
    begin match (v, p) with
    | (VInt n1, PInt n2) -> if (n1 = n2) then (p, ex) else find_first_branch v tl
    | (VBool b1, PBool b2) -> if (b1 = b2) then (p, ex) else find_first_branch v tl
    | (VList l1, PList l2) -> if (list_equal l1 l2) then (p, ex) else find_first_branch v tl
    | (VTuple l1, PTuple l2) -> if (list_equal l1 l2) then (p, ex) else find_first_branch v tl
    | (VCtor (x1, l1), PCtor(x2, l2)) -> if ((x1 = x2) && (list_equal l1 l2)) then (p, ex) else find_first_branch v tl
    | (VList l1, PCons l2) -> if (cons_equal l1 l2) then (p, ex) else find_first_branch v tl
    | (_, PVar x) -> (p, ex)
    | (_, PUnder) -> (p, ex)
    | (_, Pats pl) -> find_first_branch v ((pats_to_branch pl ex)@tl)
    | _ -> find_first_branch v tl
    end

and list_equal : value list -> pat list -> bool
= fun vs ps ->
  match (vs, ps) with
  | ([], []) -> true
  | ([], _) -> false
  | (_, []) -> false
  | (vhd::vtl, phd::ptl) ->
    begin match (vhd, phd) with 
    | (VInt n1, PInt n2) -> if (n1 = n2) then list_equal vtl ptl else false
    | (VBool b1, PBool b2) -> if (b1 = b2) then list_equal vtl ptl else false
    | (VList l1, PList l2) -> if (list_equal l1 l2) then list_equal vtl ptl else false
    | (VTuple l1, PTuple l2) -> if (list_equal l1 l2) then list_equal vtl ptl else false
    | (VCtor (x1, l1), PCtor(x2, l2)) -> if ((x1 = x2) && (list_equal l1 l2)) then list_equal vtl ptl else false
    | (VList l1, PCons l2) -> if (cons_equal l1 l2) then list_equal vtl ptl else false
    | (_, PVar x) -> list_equal vtl ptl
    | (_, PUnder) -> list_equal vtl ptl
    | _ -> false
    end

and cons_equal : value list -> pat list -> bool 
= fun vs ps ->
  match (vs, ps) with
  | ([], []) -> false
  | ([], [p]) ->
    begin match p with
    | PList [] -> true
    | PVar x -> true
    | PUnder -> true
    | _ -> false
    end
  | ([v], [p]) -> 
    begin match p with
    | PList ps -> list_equal [v] ps
    | PVar x -> true
    | PUnder -> true
    | _ -> false
    end
  | (vhd::vtl, []) -> false
  | (vhd::vtl, phd::ptl) ->
    begin match (vhd, phd) with
    | (VInt n1, PInt n2) -> if (n1 = n2) then cons_equal vtl ptl else false
    | (VBool b1, PBool b2) -> if (b1 = b2) then cons_equal vtl ptl else false
    | (VList l1, PList l2) -> if (list_equal l1 l2) then cons_equal vtl ptl else false
    | (VTuple l1, PTuple l2) -> if (list_equal l1 l2) then cons_equal vtl ptl else false
    | (VCtor (x1, l1), PCtor(x2, l2)) -> if ((x1 = x2) && (list_equal l1 l2)) then cons_equal vtl ptl else false
    | (VList l1, PCons l2) -> if (cons_equal l1 l2) then cons_equal vtl ptl else false
    | (_, PVar x) -> if (ptl = []) then true else cons_equal vtl ptl
    | (_, PUnder) -> cons_equal vtl ptl
    | _ -> false
    end
  | _ -> false

and pats_to_branch : pat list -> exp -> branch list
= fun ps ex ->
  match ps with
  | [] -> []
  | hd::tl -> (hd, ex)::(pats_to_branch tl ex)

let rec env_of_pat : value -> pat -> env -> env
= fun v p env ->
  match (v, p) with
  | (VInt n1, PInt n2) -> env
  | (VBool b1, PBool b2) -> env
  | (_, PVar x) -> update_env x v env 
  | (VList vs, PList ps) -> env_of_pat_list vs ps env
  | (VTuple vs, PTuple ps) -> env_of_pat_list vs ps env
  | (VCtor (x1, vs), PCtor (x2, ps))-> env_of_pat_list vs ps env
  | (VList vs, PCons ps) -> env_of_cons vs ps env
  | (_, PUnder) -> env
  | _ -> raise (Failure "Pattern matching failure")

and env_of_pat_list : value list -> pat list -> env -> env
= fun vs ps env ->
  match (vs, ps) with
  | ([], []) -> env
  | (vhd::vtl, phd::ptl) -> 
    let new_env = env_of_pat vhd phd env in
    env_of_pat_list vtl ptl new_env
  | _ -> raise (Failure "Pattern matching failure")

and env_of_cons : value list -> pat list -> env -> env
= fun vs ps env ->
  match (vs, ps) with
  | ([], [p]) ->
    begin match p with
    | PList [] -> env
    | PVar x -> update_env x (VList []) env
    | PUnder -> env
    | _ -> raise (Failure "Pattern matching failure")
    end
  | ([v], [p]) -> 
    begin match p with
    | PList ps -> env_of_pat_list [v] ps env
    | PVar x -> update_env x (VList [v]) env
    | PUnder -> env
    | _ -> raise (Failure "Pattern matching failure")
    end
  | (vhd::vtl, phd::ptl) -> 
    begin match phd with
    | PVar x ->
      if ptl = [] then 
        update_env x (VList vs) env
      else 
        let new_env = env_of_pat vhd phd env in
        env_of_cons vtl ptl new_env
    | _ ->
      let new_env = env_of_pat vhd phd env in
      env_of_cons vtl ptl new_env
    end
  | _ -> raise (Failure "Pattern matching failure")

let count = ref 0
let infinite_count = ref 0
let start_time = ref 0.0

let rec eval : env -> exp -> value
=fun env e -> 
  if(Unix.gettimeofday() -. !start_time >0.05) then let _ = (infinite_count:=!(infinite_count)+1) in raise (Failure "Timeout")
  else
  match e with   
  (* base *)
  | Const n -> VInt n
  | String id -> VString id
  | TRUE -> VBool true
  | FALSE -> VBool false
  | EVar x -> lookup_env x env 
  | EList (l1) -> VList (List.map (eval env) l1)
  | ETuple (l1) -> VTuple (List.map (eval env) l1)
  | ECtor (c, es) ->  VCtor (c, List.map (eval env) es)
  (* aop *)
  | ADD (x1,x2) -> 
    let v1= eval env x1 in
    let v2= eval env x2 in
    begin match v1,v2 with
    | VInt n1,VInt n2 -> VInt (n1+n2)
    |_ -> raise (Failure "Integer type error")
    end
  | SUB (x1,x2) -> 
    let v1= eval env x1 in
    let v2= eval env x2 in
    begin match v1,v2 with
    | VInt n1,VInt n2 -> VInt (n1-n2)
    |_ -> raise (Failure "Integer type error")
    end
  | MUL (x1,x2) -> 
    let v1= eval env x1 in
    let v2= eval env x2 in
    begin match v1,v2 with
    | VInt n1,VInt n2 -> VInt (n1*n2)
    |_ -> raise (Failure "Integer type error")
    end
  | DIV (x1,x2) -> 
    let v1= eval env x1 in
    let v2= eval env x2 in
    begin match v1,v2 with
    | VInt n1,VInt n2 -> VInt (n1/n2)
    |_ -> raise (Failure "Integer type error")
    end
  | MOD (x1,x2) -> 
    let v1= eval env x1 in
    let v2= eval env x2 in
    begin match v1,v2 with
    | VInt n1,VInt n2 -> VInt (n1 mod n2)
    |_ -> raise (Failure "Integer type error")
    end
  | MINUS e ->
    let v = eval env e in
    begin match v with
    |VInt n -> VInt (-n)
    |_ -> raise (Failure "Integer type error")
    end
  (*bexp*)
  | NOT x1 -> 
    let v1= eval env x1 in
    begin match v1 with
    | VBool b1 -> VBool (not b1)
    |_ -> raise (Failure "Boolean type error")
    end
  | OR (x1,x2) -> 
    let v1= eval env x1 in
    let v2= eval env x2 in
    begin match v1,v2 with
    | VBool b1,VBool b2 -> VBool (b1 || b2)
    |_ -> raise (Failure "Boolean type error")
    end 
  | AND (x1,x2) -> 
    let v1= eval env x1 in
    let v2= eval env x2 in
    begin match v1,v2 with
    | VBool b1,VBool b2 -> VBool (b1 && b2)
    |_ -> raise (Failure "Boolean type error")
    end
  | LESS (x1,x2) -> 
    let v1= eval env x1 in
    let v2= eval env x2 in
    begin match v1,v2 with
    | VInt n1,VInt n2 -> VBool (n1 < n2)
    |_ -> raise (Failure "Boolean type error")
    end
  | LARGER (x1,x2) -> 
    let v1= eval env x1 in
    let v2= eval env x2 in
    begin match v1,v2 with
    | VInt n1,VInt n2 -> VBool (n1 > n2)
    |_ -> raise (Failure "Boolean type error")
    end
  | LESSEQ (x1,x2) -> 
    let v1= eval env x1 in
    let v2= eval env x2 in
    begin match v1,v2 with
    | VInt n1,VInt n2 -> VBool (n1 <= n2)
    |_ -> raise (Failure "Boolean type error")
    end
  | LARGEREQ (x1,x2) -> 
    let v1= eval env x1 in
    let v2= eval env x2 in
    begin match v1,v2 with
    | VInt n1,VInt n2 -> VBool (n1 >= n2)
    |_ -> raise (Failure "Boolean type error")
    end
  | EQUAL (x1,x2) -> 
    let v1= eval env x1 in
    let v2= eval env x2 in
    begin match v1,v2 with
    | VInt n1, VInt n2 -> VBool (n1 = n2)
    | VBool b1, VBool b2 -> VBool (b1 = b2)
    | VList l1, VList l2 -> VBool (l1 = l2)
    | VTuple l1, VTuple l2 -> VBool (l1 = l2)
    | VCtor (x1, l1), VCtor(x2, l2) -> VBool ((x1 = x2) && (l1 = l2))
    | VString s1, VString s2 -> VBool (s1 = s2)
    |_ -> raise (Failure "Boolean type error")
    end
  | NOTEQ (x1,x2) -> 
    let v1= eval env x1 in
    let v2= eval env x2 in
    begin match v1,v2 with
    | VInt n1,VInt n2 -> VBool (n1 <> n2)
    | VBool b1,VBool b2 -> VBool (b1 <> b2)
    | VList l1, VList l2 -> VBool (l1 <> l2)
    | VTuple l1, VTuple l2 -> VBool (l1 <> l2)
    | VCtor (x1, l1), VCtor(x2, l2) -> VBool ((x1 <> x2) || (l1 <> l2))
    | VString s1, VString s2 -> VBool (s1 <> s2)
    |_ -> raise (Failure "Boolean type error")
    end
  (* lop *)
  | AT (x1,x2) ->
    let v1= eval env x1 in
    let v2= eval env x2 in
    begin match v1,v2 with
    | VList l1, VList l2 -> VList (l1 @ l2)
    |_ -> raise (Failure "List type error")
    end
  | DOUBLECOLON (x1,x2) ->
    let v1= eval env x1 in
    let v2= eval env x2 in
    begin match v2 with
    | VList l2 -> VList (v1:: l2) 
    | _ -> raise (Failure "List type error")
    end
  (* else *)
  | IF (e1,e2,e3) ->
    let v1= eval env e1 in
    if v1 = (VBool true) then eval env e2 else eval env e3
  | ELet (f, is_rec, xs, t, e1, e2) ->
    begin match xs with
    | [] ->
      (* Value binding *)
      if is_rec then
        let v1 = eval env e1 in
        begin match v1 with
        | VFun (x, e, closure) -> eval (update_env f (VFunRec (f, x, e, closure)) env) e2
        | _ -> eval (update_env f v1 env) e2
        end
      else 
        let v1 = eval env e1 in
        eval (update_env f v1 env) e2
    | _ ->
      (* Function binding *)
      let rec binding_to_funs xs e =
        begin match xs with
        | []      -> e
        | x :: xs -> EFun (x, binding_to_funs xs e)
        end 
      in
      let (x1, t1) = List.hd xs in
      let fn = 
        if is_rec then 
          let e1 = binding_to_funs (List.tl xs) e1 in
          EFix (f, (x1, t1), (List.map snd (List.tl xs)) @ [t] |> types_to_arr, e1)
        else 
          binding_to_funs xs e1 
      in
        eval (update_env f (eval env fn)  env) e2
    end 
  | EMatch (e, bs) ->
    let v = eval env e in
    let (p, ex) = find_first_branch v bs in
    (*
    let _ = print_endline("Pattern : " ^ Print.pat_to_string p) in
    *)
    let new_env = env_of_pat v p env in
    eval new_env ex
  | EFun ((x, _), e) -> VFun (x, e, env)  
  | EPFun ios -> VPFun (List.map (fun (e1, e2) -> (eval env e1, eval env e2)) ios)
  | EApp (e1, e2) ->
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    begin match v1 with
    | VFun (x, e, closure) ->
      eval (update_env x v2 closure) e
    | VFunRec (f, x, e, closure) ->
      eval (update_env f (VFunRec (f,x,e,closure)) (update_env x v2 closure)) e
    | VPFun vps ->
      begin match Util.find_first (fun (v1, _) -> v1 = v2) vps with
      | Some (_, v) -> v
      | None ->
          raise @@ Eval_error (Printf.sprintf "Non-matched value %s found with partial function:\n%s"
            (Print.string_of_value v2) (Print.string_of_value v1))
      end
    | VCtor (c,_) -> raise @@ Eval_error ("Constructor " ^ c ^ " is used in call expression")
    | _ -> raise (Failure "Function type error")
    end
  | EFix (f, (x, _), _, e) -> VFunRec (f, x, e, env)
  | Hole n -> VHole n
    
let eval_decl : decl -> env -> env
=fun decl env -> 
  match decl with
  | DData _ -> env
  | DLet (x,is_rec,args,typ,exp) -> 
    let exp = ELet (x, is_rec, args, typ, exp, EVar x) in
    update_env x (eval env exp) env

let run : prog -> bool -> env
=fun decls env_trace_option-> 
  start_time:=Unix.gettimeofday(); 
  count:=(!count)+1;
  (list_fold eval_decl decls empty_env)

