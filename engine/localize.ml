let empty_env = BatMap.empty

open Lang
open Util
open Symbol_lang
open Print
open Labeling
open Translate

let extend_env = BatMap.add

let find_env = BatMap.find

let empty_set = BatSet.empty

let extend_set = BatSet.add

let label_set = ref empty_set

let start_time = ref 0.0

let rec is_solution : prog -> examples -> bool
= fun prog examples ->
(
  match examples with
  |[] -> true
  |(exp,value)::tl ->
  (
    let result_prog = prog@[(DLet ("result",false,[],TPoly,(appify (EVar "f") exp)))] in
    try
      let result_env = Eval.run result_prog false in
      let result_value = lookup_env "result" (result_env) in
      if(result_value=value) then (is_solution prog tl) else false
    with
    | _ -> false
  )
)

let rec find_counter_examples : prog -> examples -> examples -> examples
= fun pgm exl l ->
  match exl with
  | [] -> l
  | hd::tl ->
    if (is_solution pgm [hd]) 
      then find_counter_examples pgm tl l
    else find_counter_examples pgm tl (hd::l)

let rec find_first_branch : labeled_value -> l_bl list -> l_bl
= fun v bs ->
  match bs with  
  | [] -> raise (Failure "Pattern matching failure")
  | (p, ex)::tl ->
    let p' = snd p in
    begin match (v, p') with
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

and list_equal : labeled_value list -> labeled_pat list -> bool
= fun vs ps ->
  match (vs, ps) with
  | ([], []) -> true
  | ([], _) -> false
  | (_, []) -> false
  | (vhd::vtl, phd::ptl) ->
    let phd = snd phd in
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

and cons_equal : labeled_value list -> labeled_pat list -> bool 
= fun vs ps ->
  match (vs, ps) with
  | ([], []) -> false
  | ([], [p]) ->
    let p = snd p in
    begin match p with
    | PList [] -> true
    | PVar x -> true
    | PUnder -> true
    | _ -> false
    end
  | ([v], [p]) ->
    let p = snd p in 
    begin match p with
    | PList ps -> list_equal [v] ps
    | PVar x -> true
    | PUnder -> true
    | _ -> false
    end
  | (vhd::vtl, []) -> false
  | (vhd::vtl, phd::ptl) ->
    let phd = snd phd in
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

and pats_to_branch : labeled_pat list -> labeled_exp -> l_bl list
= fun ps ex ->
  match ps with
  | [] -> []
  | hd::tl -> (hd, ex)::(pats_to_branch tl ex)

let rec env_of_pat : labeled_value -> labeled_pat -> labeled_env -> labeled_env
= fun v p env ->
  let p' = snd p in
  match (v, p') with
  | (VInt n1, PInt n2) -> env
  | (VBool b1, PBool b2) -> env
  | (_, PVar x) -> (extend_env x v env) 
  | (VList vs, PList ps) -> env_of_pat_list vs ps env
  | (VTuple vs, PTuple ps) -> env_of_pat_list vs ps env
  | (VCtor (x1, vs), PCtor (x2, ps))-> env_of_pat_list vs ps env
  | (VList vs, PCons ps) -> env_of_cons vs ps env
  | (_, PUnder) -> env
  | _ -> raise (Failure "Pattern matching failure")

and env_of_pat_list : labeled_value list -> labeled_pat list -> labeled_env -> labeled_env
= fun vs ps env ->
  let (_,ps') = list_split ps in
  match (vs, ps) with
  | ([], []) -> env
  | (vhd::vtl, phd::ptl) -> 
    let new_env = env_of_pat vhd phd env in
    env_of_pat_list vtl ptl new_env
  | _ -> raise (Failure "Pattern matching failure")

and env_of_cons : labeled_value list -> labeled_pat list -> labeled_env -> labeled_env
= fun vs ps env ->
  match (vs, ps) with
  | ([], [p]) ->
    let p = snd p in
    begin match p with
    | PList [] -> env
    | PVar x -> extend_env x (VList []) env
    | PUnder -> env
    | _ -> raise (Failure "Pattern matching failure")
    end
  | ([v], [p]) ->
    let p = snd p in 
    begin match p with
    | PList ps -> env_of_pat_list [v] ps env
    | PVar x -> extend_env x (VList [v]) env
    | PUnder -> env
    | _ -> raise (Failure "Pattern matching failure")
    end
  | (vhd::vtl, phd::ptl) ->
    let phd' = snd phd in 
    begin match phd' with
    | PVar x ->
      if ptl = [] then 
        extend_env x (VList vs) env
      else 
        let new_env = env_of_pat vhd phd env in
        env_of_cons vtl ptl new_env
    | _ ->
      let new_env = env_of_pat vhd phd env in
      env_of_cons vtl ptl new_env
    end
  | _ -> raise (Failure "Pattern matching failure")

let rec eval_abop e1 e2 env op =
  let v1 = eval_labeled_exp env e1 in
  let v2 = eval_labeled_exp env e2 in
  begin match v1,v2 with
  | VInt n1,VInt n2 -> (op n1 n2)
  |_ -> raise (Failure "int_bop error")
  end
and eval_abbop e1 e2 env op =
  let v1 = eval_labeled_exp env e1 in
  let v2 = eval_labeled_exp env e2 in
  begin match v1,v2 with
  | VInt n1,VInt n2 -> (op n1 n2)
  |_ -> raise (Failure "int_bop error")
  end
and eval_bbop e1 e2 env op =
  let v1 = eval_labeled_exp env e1 in
  let v2 = eval_labeled_exp env e2 in
  begin match v1,v2 with
  | VBool b1,VBool b2 -> (op b1 b2)
  |_ -> raise (Failure "int_bop error")
  end

and eval_labeled_exp : labeled_env -> labeled_exp -> labeled_value
= fun env (l,e) ->
  let _ = if(Unix.gettimeofday() -. !start_time > 0.5) then raise(Failure "Timeout") in
  let _ = label_set:= extend_set l !label_set in
  match e with
  | Const n -> VInt n
  | TRUE -> VBool true
  | FALSE -> VBool false
  | EVar x -> find_env x env
  | EList el -> VList (list_map (eval_labeled_exp env) el)
  | ETuple el -> VTuple (list_map (eval_labeled_exp env) el)
  | ECtor (id,el) -> VCtor (id,list_map (eval_labeled_exp env) el)
  | ADD (e1,e2) -> VInt (eval_abop e1 e2 env (+))
  | SUB (e1,e2) -> VInt (eval_abop e1 e2 env (-))
  | MUL (e1,e2) -> VInt (eval_abop e1 e2 env ( * ))
  | DIV (e1,e2) -> VInt (eval_abop e1 e2 env (/))
  | MOD (e1,e2) -> VInt (eval_abop e1 e2 env (mod))
  | MINUS e1 -> 
    begin match (eval_labeled_exp env e1) with 
    | VInt n1 -> VInt ((-1)*n1) 
    |_ -> raise (Failure "int_uop error")
    end
  | NOT e1 ->
    begin match (eval_labeled_exp env e1) with 
    | VBool b1 -> VBool (not b1) 
    |_ -> raise (Failure "int_uop error")
    end
  | OR (e1,e2) -> VBool (eval_bbop e1 e2 env (||))
  | AND (e1,e2) -> VBool (eval_bbop e1 e2 env (&&))
  | LESS (e1,e2) -> VBool (eval_abbop e1 e2 env (<))
  | LARGER (e1,e2) -> VBool (eval_abbop e1 e2 env (>))
  | LESSEQ (e1,e2) -> VBool (eval_abbop e1 e2 env (<=))
  | LARGEREQ (e1,e2) -> VBool (eval_abbop e1 e2 env (>=))
  | EQUAL (e1,e2) -> 
    let v1 = eval_labeled_exp env e1 in
    let v2 = eval_labeled_exp env e2 in
    begin match v1,v2 with
    | VInt n1, VInt n2 -> VBool (n1 = n2)
    | VBool b1, VBool b2 -> VBool (b1 = b2)
    | VList l1, VList l2 -> VBool (l1 = l2)
    | VTuple l1, VTuple l2 -> VBool (l1 = l2)
    | VCtor (x1, l1), VCtor(x2, l2) -> VBool ((x1 = x2) && (l1 = l2))
    |_ -> raise (Failure "equal error")
    end
  | NOTEQ (e1,e2) -> 
    let v1 = eval_labeled_exp env e1 in
    let v2 = eval_labeled_exp env e2 in
    begin match v1,v2 with
    | VInt n1, VInt n2 -> VBool (n1 != n2)
    | VBool b1, VBool b2 -> VBool (b1 != b2)
    | VList l1, VList l2 -> VBool (l1 != l2)
    | VTuple l1, VTuple l2 -> VBool (l1 != l2)
    | VCtor (x1, l1), VCtor(x2, l2) -> VBool ((x1 != x2) || (l1 != l2))
    |_ -> raise (Failure "equal error")
    end
  | AT (e1,e2) ->
    let v1= eval_labeled_exp env e1 in
    let v2= eval_labeled_exp env e2 in
    begin match v1,v2 with
    | VList l1, VList l2 -> VList (l1 @ l2)
    | _ -> raise (Failure "List type error")
    end
  | DOUBLECOLON (e1,e2) ->
    let v1= eval_labeled_exp env e1 in
    let v2= eval_labeled_exp env e2 in
    begin match v2 with
    | VList l2 -> VList (v1 :: l2) 
    | _ -> raise (Failure "List type error")
    end
  | EFun ((x,_),e) -> VFun (x, e, env, empty_env)  
  | IF (e1,e2,e3) ->
    let v1= eval_labeled_exp env e1 in
    begin match v1 with
    | VBool true -> eval_labeled_exp env e2
    | VBool false -> eval_labeled_exp env e3
    | _ -> raise (Failure "if type error")
    end
  | EApp (e1,e2) ->
    let v1 = eval_labeled_exp env e1 in
    let v2 = eval_labeled_exp env e2 in
    begin match v1 with
    | VFun (x,e,env',_) -> 
      eval_labeled_exp (extend_env x v2 env') e
    | VFunRec (f,x,e,env',_) ->
      let env' = extend_env x v2 env' in
      let env' = extend_env f v1 env' in
      eval_labeled_exp env' e
    | _ -> raise (Failure "Function call error")
    end
  | ELet (f,is_rec,args,t,e1,e2) ->
    begin match args with
    |[] ->
      if is_rec then
        let v1 = eval_labeled_exp env e1 in
        begin match v1 with
        | VFun (x,e,env',se) -> 
          let env' = extend_env f (VFunRec (f,x,e,env',se)) env' in
          eval_labeled_exp env' e2
        |_ -> eval_labeled_exp (extend_env f v1 env) e2
        end
      else
        let v1 = eval_labeled_exp env e1 in 
        eval_labeled_exp (extend_env f v1 env) e2
    |_ ->
      let rec binding xs e = 
      begin match xs with
      | [] -> e
      | hd::tl -> (-1,EFun (hd,binding tl e))
      end in
      let ((x1,_)::tl) = args in
      let vf =
        if is_rec then
          let e = binding tl e1 in
          VFunRec (f,x1,e,env,empty_env)
        else
          let e = binding tl e1 in
          VFun(x1,e,env,empty_env)
      in
      eval_labeled_exp (extend_env f vf env) e2
    end
  | EMatch (e,bl) -> 
    let v = eval_labeled_exp env e in
    let (p,ex) = find_first_branch v bl in
    let new_env = env_of_pat v p env in
    eval_labeled_exp new_env ex
  | _ -> raise(Failure "Hole")

let eval_labeled_decl : labeled_decl -> labeled_env -> labeled_env
= fun decl env ->
  match decl with
  |DData (id,ctors) -> env
  |DLet (f,is_rec,args,t,l_e) -> 
    let exp = (-1,ELet (f,is_rec,args,t,l_e,(-1,EVar f)))in
    extend_env f (eval_labeled_exp env exp) env

let gen_pgm_label : labeled_prog -> example -> label BatSet.t
= fun l_pgm (input,_) -> 
  let output = labeling_exp (appify (EVar "f") input) in
  let l_pgm = l_pgm@[DLet ("@",false,[],TPoly,output)] in
  let _ = start_time := Unix.gettimeofday() in
  let _ = label_set :=empty_set in
  try
    let _ = list_fold eval_labeled_decl l_pgm empty_env in
    !label_set
  with
  | _ -> !label_set

let gen_counter_label : labeled_prog -> example -> label BatSet.t -> label BatSet.t
= fun l_pgm example set->
  let (result_set) = gen_pgm_label l_pgm example in
  BatSet.union result_set set

let gen_candidate_pgm : prog -> examples -> (int * prog) BatSet.t
= fun pgm examples ->
  let rank = cost pgm in
  let l_pgm = labeling_prog pgm in
  let counter_examples = find_counter_examples pgm examples [] in
  let label_set = list_fold (gen_counter_label l_pgm) counter_examples empty_set in
  let _ = print_endline (string_of_int (BatSet.cardinal label_set)) in
  let candidate_set = BatSet.fold (
    fun label set ->
      let hole_pgm = gen_hole_pgm l_pgm label in
      let candidate_pgm = unlabeling_prog hole_pgm in
      let rank' = cost candidate_pgm in
      if (is_closed candidate_pgm) then set else extend_set (rank-rank', candidate_pgm) set
  ) label_set empty_set
  in
  let _ = print_endline (string_of_int (BatSet.cardinal candidate_set)) in
  candidate_set


let localization : prog -> examples -> (int * prog) BatSet.t
= fun pgm examples ->
  let candidate_set = gen_candidate_pgm pgm examples in
  candidate_set