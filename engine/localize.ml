open Lang
open Util
open Symbol_lang
open Print
open Labeling
open Translate

module Memoization = struct
  type t = (string,bool) BatMap.t
  let empty = BatMap.empty
  let exist x t = BatMap.mem x t
  let add p b t = BatMap.add p b t
  let print t = BatMap.iter ( fun p _ -> print_endline p ) t
end

let empty_env = BatMap.empty

let extend_env (x,t) env = BatMap.add x t env

let find_env x env = BatMap.find x env

let empty_set = BatSet.empty

let extend_set t set = BatSet.add t set

let symbol_num = ref 0
let fresh_symbol () = (symbol_num := !symbol_num +1; !symbol_num)

let label_set = ref BatSet.empty

let store = ref Memoization.empty

(*
  functions for interpreting exp
*)
let rec is_poly : typ -> bool
= fun t ->
  match t with
  | TInt -> false
  | TBool -> false
  | TPoly -> true
  | TString -> false
  | TBase x -> false
  | TList t1 -> is_poly t1 
  | TTuple ts -> false
  | TCtor (t1, ts) -> false
  | TArr (t1, t2) -> (is_poly t1) || (is_poly t2)
  | TVar x -> false

let type_of_bop : typ -> typ -> typ
= fun t1 t2 ->
  if is_poly t1 then t2
  else if is_poly t2 then t1
  else (
    if (t1 <> t2) then raise (Failure "NOTEQ Error") else t1
  )

let type_of_cons : typ -> typ -> typ
= fun t1 t2 ->
  if is_poly t1 then
    match t1 with
    | TList t -> t
    | TPoly -> TPoly
  else t1

let rec types_to_arr : typ list -> typ
= fun ts ->
  match ts with
  | []  -> raise (Failure "(types_to_arr) empty type list provided")
  | [t] -> t
  | t :: ts -> TArr (t, types_to_arr ts)

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
    | _ -> (*let _ = Printf.fprintf oc "is_solution error" in*) false
  )
)

let rec binding_to_funs : arg list -> labeled_exp -> labeled_exp
= fun args e ->
  match args with
  | [] -> e
  | hd::tl -> 
    (* make dummy expression for function *)
    (-1, EFun (hd, binding_to_funs tl e))

let rec find_first_branch : labeled_value -> l_bl list -> l_bl
= fun v bs ->
  match bs with 
  | [] -> raise (Failure "Pattern matching failure")
  | ((l, p), ex)::tl ->
    begin match (v, p) with
    | (VInt n1, PInt n2) -> if (n1 = n2) then ((l, p), ex) else find_first_branch v tl
    | (VBool b1, PBool b2) -> if (b1 = b2) then ((l, p), ex) else find_first_branch v tl
    | (VList l1, PList l2) -> if (list_equal l1 l2) then ((l, p), ex) else find_first_branch v tl
    | (VTuple l1, PTuple l2) -> if (list_equal l1 l2) then ((l, p), ex) else find_first_branch v tl
    | (VCtor (x1, l1), PCtor(x2, l2)) -> if ((x1 = x2) && (list_equal l1 l2)) then ((l, p), ex) else find_first_branch v tl
    | (VList l1, PCons l2) -> if (cons_equal l1 l2) then ((l, p), ex) else find_first_branch v tl
    | (_, PVar x) -> ((l, p), ex)
    | (_, PUnder) -> ((l, p), ex)
    | (_, Pats pl) -> find_first_branch v ((pats_to_branch pl ex)@tl)
    | _ -> find_first_branch v tl
    end

and list_equal : labeled_value list -> labeled_pat list -> bool
= fun vs ps ->
  match (vs, ps) with
  | ([], []) -> true
  | ([], _) -> false
  | (_, []) -> false
  | (vhd::vtl, (l, phd)::ptl) ->
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
  | ([], [(l, p)]) ->
    begin match p with
    | PList [] -> true
    | PVar x -> true
    | PUnder -> true
    | _ -> false
    end
  | ([v], [(l, p)]) -> 
    begin match p with
    | PList ps -> list_equal [v] ps
    | PVar x -> true
    | PUnder -> true
    | _ -> false
    end
  | (vhd::vtl, []) -> false
  | (vhd::vtl, (l, phd)::ptl) ->
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

and pats_to_branch : labeled_pat list-> labeled_exp -> l_bl list
= fun ps ex ->
  match ps with
  | [] -> []
  | hd::tl -> (hd, ex)::(pats_to_branch tl ex)

(*
  generate constraint for value
*)
let rec gen_value_constraint : formula -> symbol_type_table -> symbolic_variable -> value -> (formula * symbol_type_table)
= fun formula type_table symbol value ->
  match value with
  | VHole n ->
    let formula = extend_set (symbol, Unknown) formula in
    let type_table = extend_env (symbol,TPoly) type_table in
    (formula, type_table)
  | VInt n -> 
    let formula = extend_set (symbol, Int n) formula in
    let type_table = extend_env (symbol,TInt) type_table in
    (formula, type_table)
  | VBool b ->
    let formula = extend_set (symbol, Bool b) formula in
    let type_table = extend_env (symbol,TBool) type_table in
    (formula, type_table)
  | VList vs ->
    let (symbols, typs, formula, type_table) = gen_value_list_constraint formula type_table vs in
    let formula = extend_set (symbol, List symbols) formula in
    let type_table = 
      try 
        extend_env (symbol, TList (List.hd typs)) type_table
      with _ -> extend_env (symbol, TList TPoly) type_table
    in
    (formula, type_table)
  | VTuple vs ->
    let (symbols, typs, formula, type_table) = gen_value_list_constraint formula type_table vs in
    let formula = extend_set (symbol, Tuple symbols) formula in
    let type_table = extend_env (symbol, TTuple typs) type_table in
    (formula, type_table)
  | VFun (x, e, env) ->
    let formula = extend_set (symbol, Fun (x, labeling_exp e)) formula in
    let type_table = extend_env (symbol, TArr(TPoly, TPoly)) type_table in
    (formula, type_table)
  | VFunRec (f, x, e, env) ->
    let formula = extend_set (symbol, FunRec (f, x, labeling_exp e)) formula in
    let type_table = extend_env (symbol, TArr(TPoly, TPoly)) type_table in
    (formula, type_table)
  (*
  | VString of string
  | VCtor of id * value list
  | VPFun of (value * value) list
  | VHole of int
  *)
  | _ -> raise (Failure "Value Constraint Generation Failure")

and gen_value_list_constraint : formula -> symbol_type_table -> value list -> (symbolic_value list * typ list * formula * symbol_type_table)
= fun formula type_table value_list ->
  List.fold_left (
    fun (symbols, typs, formula, type_table) value ->
      let symbol = fresh_symbol() in
      let (formula, type_table) = gen_value_constraint formula type_table symbol value in
      (symbols@[Symbol symbol], typs@[(find_env symbol type_table)], formula, type_table)
  ) ([], [], formula, type_table) value_list

(*
  generate constraint for pattern
*)
let rec gen_pat_constraint : labeled_env -> symbolic_env -> formula -> (labeled_value * labeled_pat) -> symbol_type_table -> (labeled_env * symbolic_env * symbolic_variable * formula * symbol_type_table)
= fun labeled_env symbol_env formula (v, (l,p)) type_table ->
  let symbol = fresh_symbol() in
  match (v, p) with 
  | (VInt n1, PInt n2) -> 
    let formula = extend_set (symbol, Int n1) formula in
    let type_table = extend_env (symbol, TInt) type_table in
    (labeled_env, symbol_env, symbol, formula, type_table)
  | (VBool b1, PBool b2) -> 
    let formula = extend_set (symbol, Bool b1) formula in
    let type_table = extend_env (symbol, TBool) type_table in
    (labeled_env, symbol_env, symbol, formula, type_table)
  | (VList vs, PList ps) -> 
    let (labeled_env, symbol_env, symbols, typs, formula, type_table) = gen_pat_list_constraint labeled_env symbol_env formula (vs, ps) type_table in
    let formula = extend_set (symbol, List symbols) formula in
    let type_table = 
      try 
        extend_env (symbol, TList (List.hd typs)) type_table 
      with _ -> extend_env (symbol, TList TPoly) type_table
    in
    (labeled_env, symbol_env, symbol, formula, type_table)
  | (VTuple vs, PTuple ps) -> 
    let (labeled_env, symbol_env, symbols, typs, formula, type_table) = gen_pat_list_constraint labeled_env symbol_env formula (vs, ps) type_table in
    let formula = extend_set (symbol, Tuple symbols) formula in
    let type_table = extend_env (symbol, TTuple typs) type_table in
    (labeled_env, symbol_env, symbol, formula, type_table)
  (*
  | (VCtor (x1, l1), PCtor(x2, l2)) -> if ((x1 = x2) && (list_equal l1 l2)) then ((l, p), ex) else find_first_branch v tl
  *)
  | (VList vs, PCons ps) -> 
    begin match (vs, ps) with
    | (_, []) -> raise (Failure "Pattern_Cons Constraint Generation Failure")
    | ([], phd::ptl) -> 
      if ptl = [] then
        gen_pat_constraint labeled_env symbol_env formula (VList vs, phd) type_table 
      else 
        raise (Failure "Pattern_Cons Constraint Generation Failure")
    | (vhd::vtl, phd::ptl) ->
      if ptl = [] then
        gen_pat_constraint labeled_env symbol_env formula (VList vs, phd) type_table 
      else
        let (labeled_env, symbol_env, symbol1, formula, type_table) = gen_pat_constraint labeled_env symbol_env formula (vhd, phd) type_table in
        let (labeled_env, symbol_env, symbol2, formula, type_table) = gen_pat_constraint labeled_env symbol_env formula (VList vtl, (l, PCons ptl)) type_table in
        let formula = extend_set (symbol, Cons(Symbol symbol1, Symbol symbol2)) formula in
        let type_table = extend_env (symbol, TList (find_env symbol1 type_table)) type_table in
        (labeled_env, symbol_env, symbol, formula, type_table)
    end
  | (_, PVar x) -> 
    let labeled_env = extend_env (x, v) labeled_env in
    let symbol_env = extend_env (x, symbol) symbol_env in
    let (formula, type_table) = gen_value_constraint formula type_table symbol (unlabeling_value v) in
    (labeled_env, symbol_env, symbol, formula, type_table)
  | (_, PUnder) -> 
    let (formula, type_table) = gen_value_constraint formula type_table symbol (unlabeling_value v) in
    (labeled_env, symbol_env, symbol, formula, type_table)
  | _ -> raise (Failure "Pattern Constraint Generation Failure")

and gen_pat_list_constraint : labeled_env -> symbolic_env -> formula -> (labeled_value list * labeled_pat list) -> symbol_type_table -> (labeled_env * symbolic_env * symbolic_value list * typ list * formula * symbol_type_table)
= fun labeled_env symbol_env formula (vs, ps) type_table ->
  List.fold_left2 (
    fun (labeled_env, symbol_env, symbols, typs, formula, type_table) v p ->
      let (labeled_env, symbol_env, symbol, formula, type_table) = gen_pat_constraint labeled_env symbol_env formula (v, p) type_table in
      (labeled_env, symbol_env, symbols@[Symbol symbol], typs@[(find_env symbol type_table)], formula, type_table)
  ) (labeled_env, symbol_env, [], [], formula, type_table) vs ps

(*
  generate counter examples' constraint set for expressions
*)
let start_time = ref 0.0
let flag = ref 0
let pgm_string = ref ""

let eval_intbop : (int -> int -> int) -> labeled_value -> labeled_value -> labeled_value
= fun bop v1 v2 ->
  match (v1, v2) with
    | VInt n1, VInt n2 -> VInt (bop n1 n2)
    | VHole n1, VInt n2 -> VHole n1
    | VInt n1, VHole n2 -> VHole n2
    | VHole n1, VHole n2 -> VHole n1
    | _ -> raise (Failure "integer bop error")

let rec gen_exp_constraint : labeled_env -> symbolic_env -> formula -> labeled_exp -> symbol_type_table -> (labeled_value * symbolic_variable * formula * symbol_type_table)
= fun labeled_env symbol_env formula (l,exp) type_table ->
  if(Unix.gettimeofday() -. !start_time >1.0) 
    then 
      let _ = Memoization.add !pgm_string false !store in
      let _ = flag := 1 in raise (Failure "Timeout")
  else
  let symbol = fresh_symbol() in
  (* Accumulate the execution trace *)
  label_set := BatSet.add l !label_set;
  match exp with
  | Hole n ->
    let type_table = extend_env (symbol, TPoly) type_table in
    let formula = extend_set (symbol, Unknown) formula in
    (VHole n,symbol,formula,type_table)
  | Const n ->
    let type_table = extend_env (symbol,TInt) type_table in
    let formula = extend_set (symbol,Int n) formula in
    (VInt n,symbol,formula,type_table)
  | TRUE ->
    let type_table = extend_env (symbol,TBool) type_table in
    let formula = extend_set (symbol,Bool true) formula in
    (VBool true,symbol,formula,type_table)
  | FALSE ->
    let type_table = extend_env (symbol,TBool) type_table in
    let formula = extend_set (symbol,Bool false) formula in
    (VBool false,symbol,formula,type_table)
  | EVar x ->
    let x_symbol = find_env x symbol_env in
    let type_table = extend_env (symbol,(find_env x_symbol type_table)) type_table in
    let formula = extend_set (symbol,Symbol x_symbol) formula in
    let value = (find_env x labeled_env) in
    (value,symbol,formula,type_table)
  | EList es -> 
    let (values, symbols, typs, formula, type_table) = gen_exp_list_constraint labeled_env symbol_env formula es type_table in
    let formula = extend_set (symbol, List symbols) formula in
    let type_table = 
      try 
        extend_env (symbol, TList (List.find (fun t -> t <> TPoly) typs)) type_table
      with _ -> extend_env (symbol, TList TPoly) type_table
    in
    (VList values, symbol, formula, type_table)
  | ETuple es -> 
    let (values, symbols, typs, formula, type_table) = gen_exp_list_constraint labeled_env symbol_env formula es type_table in
    let formula = extend_set (symbol, Tuple symbols) formula in
    let type_table = extend_env (symbol, TTuple typs) type_table in
    (VTuple values, symbol, formula, type_table)
  (*
  | ECtor (x, es) -> 
    let (w', ttbl', evs) = formula_of_list es lenv eenv (w, ttbl, []) in
    let w' = BatMap.add ev (Ctor (x, evs)) w' in
    let tbase = BatMap.find x !tbase_tbl in
    let typ_list = type_of_evar_list evs ttbl' in
    let ttbl' = BatMap.add ev (TCtor (tbase, typ_list)) ttbl' in
    (w', ttbl') 
  *)
  | ADD (e1, e2) ->
    let (value1,symbol1,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e1 type_table in
    let (value2,symbol2,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e2 type_table in
    let value = eval_intbop (+) value1 value2 in
    let symbol_value = Add (Symbol symbol1, Symbol symbol2) in
    let type_table = extend_env (symbol1, TInt) type_table in
    let type_table = extend_env (symbol2, TInt) type_table in
    let type_table = extend_env (symbol,TInt) type_table in
    let formula = extend_set (symbol,symbol_value) formula in
    (value,symbol,formula,type_table)
  | SUB (e1, e2) ->
    let (value1,symbol1,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e1 type_table in
    let (value2,symbol2,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e2 type_table in
    let value = eval_intbop (-) value1 value2 in
    let symbol_value = Sub (Symbol symbol1, Symbol symbol2) in
    let type_table = extend_env (symbol1, TInt) type_table in
    let type_table = extend_env (symbol2, TInt) type_table in
    let type_table = extend_env (symbol,TInt) type_table in
    let formula = extend_set (symbol,symbol_value) formula in
    (value,symbol,formula,type_table)
  | MUL (e1, e2) -> 
    let (value1,symbol1,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e1 type_table in
    let (value2,symbol2,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e2 type_table in
    let value = eval_intbop ( * ) value1 value2 in
    let symbol_value = Mul (Symbol symbol1, Symbol symbol2) in
    let type_table = extend_env (symbol1, TInt) type_table in
    let type_table = extend_env (symbol2, TInt) type_table in
    let type_table = extend_env (symbol,TInt) type_table in
    let formula = extend_set (symbol,symbol_value) formula in
    (value,symbol,formula,type_table)
  | DIV (e1, e2) ->
    let (value1,symbol1,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e1 type_table in
    let (value2,symbol2,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e2 type_table in
    let value = eval_intbop (/) value1 value2 in
    let symbol_value = Div (Symbol symbol1, Symbol symbol2) in
    let type_table = extend_env (symbol1, TInt) type_table in
    let type_table = extend_env (symbol2, TInt) type_table in
    let type_table = extend_env (symbol,TInt) type_table in
    let formula = extend_set (symbol,symbol_value) formula in
    (value,symbol,formula,type_table)
  | MOD (e1, e2) ->
    let (value1,symbol1,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e1 type_table in
    let (value2,symbol2,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e2 type_table in
    let value = eval_intbop ( mod ) value1 value2 in
    let symbol_value = Mod (Symbol symbol1, Symbol symbol2) in
    let type_table = extend_env (symbol1, TInt) type_table in
    let type_table = extend_env (symbol2, TInt) type_table in
    let type_table = extend_env (symbol,TInt) type_table in
    let formula = extend_set (symbol,symbol_value) formula in
    (value,symbol,formula,type_table)
  | MINUS e1 ->
    let (value1,symbol1,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e1 type_table in
    let value = 
      match value1 with
      | VInt n1 -> VInt ((-1)*n1)
      | VHole n1 -> VHole n1
      | _ -> raise (Failure "MINUS Error")
    in
    let symbol_value = Minus (Symbol symbol1) in
    let type_table = extend_env (symbol1, TInt) type_table in
    let type_table = extend_env (symbol,TInt) type_table in
    let formula = extend_set (symbol,symbol_value) formula in
    (value,symbol,formula,type_table)
  | NOT e1 ->
    let (value1,symbol1,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e1 type_table in
    let value = 
      match value1 with
      | VBool b1 -> VBool (not b1)
      | VHole n1 -> VHole n1
      | _ -> raise (Failure "NOT Error")
    in
    let symbol_value = Minus (Symbol symbol1) in
    let type_table = extend_env (symbol1, TBool) type_table in
    let type_table = extend_env (symbol,TBool) type_table in
    let formula = extend_set (symbol,symbol_value) formula in
    (value,symbol,formula,type_table)
  | OR (e1, e2) ->
    let (value1,symbol1,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e1 type_table in
    let (value2,symbol2,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e2 type_table in
    let value = 
      match (value1, value2) with
      | VBool b1, VBool b2 -> VBool (b1 || b2)
      | VHole n1, VBool b2 -> VHole n1
      | VBool b1, VHole n2 -> VHole n2
      | VHole n1, VHole n2 -> VHole n1
      | _ -> raise (Failure "OR Error")
    in
    let symbol_value = Or (Symbol symbol1, Symbol symbol2) in
    let type_table = extend_env (symbol1, TBool) type_table in
    let type_table = extend_env (symbol2, TBool) type_table in
    let type_table = extend_env (symbol,TBool) type_table in
    let formula = extend_set (symbol,symbol_value) formula in
    (value,symbol,formula,type_table)
  | AND (e1, e2) ->
    let (value1,symbol1,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e1 type_table in
    let (value2,symbol2,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e2 type_table in
    let value = 
      match (value1, value2) with
      | VBool b1, VBool b2 -> VBool (b1 && b2)
      | VHole n1, VBool b2 -> VHole n1
      | VBool b1, VHole n2 -> VHole n2
      | VHole n1, VHole n2 -> VHole n1
      | _ -> raise (Failure "AND Error")
    in
    let symbol_value = And (Symbol symbol1, Symbol symbol2) in
    let type_table = extend_env (symbol1, TBool) type_table in
    let type_table = extend_env (symbol2, TBool) type_table in
    let type_table = extend_env (symbol,TBool) type_table in
    let formula = extend_set (symbol,symbol_value) formula in
    (value,symbol,formula,type_table)
  | LESS (e1, e2) ->
    let (value1,symbol1,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e1 type_table in
    let (value2,symbol2,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e2 type_table in
    let value = 
      match (value1, value2) with
      | VInt n1, VInt n2 -> VBool (n1 < n2)
      | VHole n1, VInt b2 -> VHole n1
      | VInt b1, VHole n2 -> VHole n2
      | VHole n1, VHole n2 -> VHole n1
      | _ -> raise (Failure "LESS Error")
    in
    let symbol_value = Lt (Symbol symbol1, Symbol symbol2) in
    let type_table = extend_env (symbol1, TInt) type_table in
    let type_table = extend_env (symbol2, TInt) type_table in
    let type_table = extend_env (symbol,TBool) type_table in
    let formula = extend_set (symbol,symbol_value) formula in
    (value,symbol,formula,type_table)
  | LARGER (e1, e2) ->
    let (value1,symbol1,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e1 type_table in
    let (value2,symbol2,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e2 type_table in
    let value = 
      match (value1, value2) with
      | VInt n1, VInt n2 -> VBool (n1 > n2)
      | VHole n1, VInt b2 -> VHole n1
      | VInt b1, VHole n2 -> VHole n2
      | VHole n1, VHole n2 -> VHole n1
      | _ -> raise (Failure "LARGER Error")
    in
    let symbol_value = Gt (Symbol symbol1, Symbol symbol2) in
    let type_table = extend_env (symbol1, TInt) type_table in
    let type_table = extend_env (symbol2, TInt) type_table in
    let type_table = extend_env (symbol,TBool) type_table in
    let formula = extend_set (symbol,symbol_value) formula in
    (value,symbol,formula,type_table)
  | LESSEQ (e1, e2) ->
    let (value1,symbol1,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e1 type_table in
    let (value2,symbol2,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e2 type_table in
    let value = 
      match (value1, value2) with
      | VInt n1, VInt n2 -> VBool (n1 <= n2)
      | VHole n1, VInt b2 -> VHole n1
      | VInt b1, VHole n2 -> VHole n2
      | VHole n1, VHole n2 -> VHole n1
      | _ -> raise (Failure "LESSEQ Error")
    in
    let symbol_value = Le (Symbol symbol1, Symbol symbol2) in
    let type_table = extend_env (symbol1, TInt) type_table in
    let type_table = extend_env (symbol2, TInt) type_table in
    let type_table = extend_env (symbol,TBool) type_table in
    let formula = extend_set (symbol,symbol_value) formula in
    (value,symbol,formula,type_table)
  | LARGEREQ (e1, e2) ->
    let (value1,symbol1,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e1 type_table in
    let (value2,symbol2,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e2 type_table in
    let value = 
      match (value1, value2) with
      | VInt n1, VInt n2 -> VBool (n1 >= n2)
      | VHole n1, VInt b2 -> VHole n1
      | VInt b1, VHole n2 -> VHole n2
      | VHole n1, VHole n2 -> VHole n1
      | _ -> raise (Failure "LARGEREQ Error")
    in
    let symbol_value = Ge (Symbol symbol1, Symbol symbol2) in
    let type_table = extend_env (symbol1, TInt) type_table in
    let type_table = extend_env (symbol2, TInt) type_table in
    let type_table = extend_env (symbol,TBool) type_table in
    let formula = extend_set (symbol,symbol_value) formula in
    (value,symbol,formula,type_table)
  | EQUAL (e1, e2) ->
    let (value1,symbol1,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e1 type_table in
    let (value2,symbol2,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e2 type_table in
    let value = 
      match (value1, value2) with
      | VHole n1, _-> VHole n1
      | _, VHole n2 -> VHole n2
      | VInt n1, VInt n2 -> VBool (n1 = n2)
      | VBool b1, VBool b2 -> VBool (b1 = b2)
      | VList l1, VList l2 -> VBool (l1 = l2)
      | VTuple l1, VTuple l2 -> VBool (l1 = l2)
      | VCtor (x1, l1), VCtor(x2, l2) -> VBool ((x1 = x2) && (l1 = l2))
      | _ -> raise (Failure "EQUAL Error")
    in
    let t = type_of_bop (find_env symbol1 type_table) (find_env symbol2 type_table) in
    let symbol_value = Eq (Symbol symbol1, Symbol symbol2) in
    let type_table = extend_env (symbol1, t) type_table in
    let type_table = extend_env (symbol2, t) type_table in
    let type_table = extend_env (symbol,TBool) type_table in
    let formula = extend_set (symbol,symbol_value) formula in
    (value,symbol,formula,type_table)
  | NOTEQ (e1, e2) ->
    let (value1,symbol1,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e1 type_table in
    let (value2,symbol2,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e2 type_table in
    let value = 
      match (value1, value2) with
      | VHole n1, _-> VHole n1
      | _, VHole n2 -> VHole n2
      | VInt n1,VInt n2 -> VBool (n1 <> n2)
      | VBool b1,VBool b2 -> VBool (b1 <> b2)
      | VList l1, VList l2 -> VBool (l1 <> l2)
      | VTuple l1, VTuple l2 -> VBool (l1 <> l2)
      | VCtor (x1, l1), VCtor(x2, l2) -> VBool ((x1 <> x2) || (l1 <> l2))
      | _ -> raise (Failure "EQUAL Error")
    in
    let t = type_of_bop (find_env symbol1 type_table) (find_env symbol2 type_table) in
    let symbol_value = NEq (Symbol symbol1, Symbol symbol2) in
    let type_table = extend_env (symbol1, t) type_table in
    let type_table = extend_env (symbol2, t) type_table in
    let type_table = extend_env (symbol,TBool) type_table in
    let formula = extend_set (symbol,symbol_value) formula in
    (value,symbol,formula,type_table)
  | AT (e1, e2) -> 
    let (value1,symbol1,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e1 type_table in
    let (value2,symbol2,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e2 type_table in
    let value = 
      match (value1, value2) with
      | VList l1, VList l2 -> VList (l1@l2)
      | VList l1, VHole n2 -> VHole n2
      | VHole n1, VList l2 -> VHole n1
      | VHole n1, VHole n2 -> VHole n1
      | _ -> raise (Failure "AT Error")
    in
    let t = type_of_bop (find_env symbol1 type_table) (find_env symbol2 type_table) in
    let symbol_value = At (Symbol symbol1, Symbol symbol2) in
    let type_table = extend_env (symbol1,t) type_table in
    let type_table = extend_env (symbol2,t) type_table in
    let type_table = extend_env (symbol,t) type_table in
    let formula = extend_set (symbol,symbol_value) formula in
    (value,symbol,formula,type_table)
  | DOUBLECOLON (e1, e2) ->
    let (value1,symbol1,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e1 type_table in
    let (value2,symbol2,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e2 type_table in
    let value =
      match (value1, value2) with
      | _, VHole n2 -> VHole n2
      | v1, VList l2 -> VList (v1::l2)
      | VHole n1, VHole n2 -> VHole n1
      | _ -> raise (Failure "DOUBLECOLON Error")
    in
    let t = type_of_cons (find_env symbol1 type_table) (find_env symbol2 type_table) in
    let symbol_value = Cons (Symbol symbol1, Symbol symbol2) in
    let type_table = extend_env (symbol1,t) type_table in
    let type_table = extend_env (symbol2,TList t) type_table in
    let type_table = extend_env (symbol,TList t) type_table in
    let formula = extend_set (symbol,symbol_value) formula in
    (value,symbol,formula,type_table)
  | IF (e1, e2, e3) ->
    let (value1,symbol1,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula e1 type_table in
    begin match value1 with
    | VBool true ->
      let (value2, symbol2, formula, type_table) = gen_exp_constraint labeled_env symbol_env formula e2 type_table in
      let t = find_env symbol2 type_table in
      let symbol3 = fresh_symbol() in
      let symbol_value = If(Symbol symbol1,Symbol symbol2,Symbol symbol3) in
      let type_table = extend_env (symbol,t) type_table in
      let type_table = extend_env (symbol3,t) type_table in
      let formula = extend_set (symbol,symbol_value) formula in
      let formula = extend_set (symbol3,Unknown) formula in
      (value2,symbol,formula,type_table)
    | VBool false ->
      let (value3, symbol3, formula, type_table) = gen_exp_constraint labeled_env symbol_env formula e3 type_table in
      let t = find_env symbol3 type_table in
      let symbol2 = fresh_symbol() in
      let symbol_value = If(Symbol symbol1,Symbol symbol2,Symbol symbol3) in
      let type_table = extend_env (symbol,t) type_table in
      let type_table = extend_env (symbol2,t) type_table in
      let formula = extend_set (symbol,symbol_value) formula in
      let formula = extend_set (symbol2,Unknown) formula in
      (value3,symbol,formula,type_table)
    | _ -> raise (Failure ("IF Error " ^ (Print.labeled_value_to_string value1)))
    end
  (* ?? *)
  | EFun ((x, t), e) -> 
    let formula = extend_set (symbol, Fun (x, e)) formula in
    let type_table = extend_env (symbol, TArr(t, TPoly)) type_table in
    (VFun (x, e, labeled_env, symbol_env), symbol, formula, type_table)
  | EFix (f, (x, t), typ, e) ->
    let formula = extend_set (symbol, FunRec (f, x, e)) formula in
    let type_table = extend_env (symbol, TArr(t, TPoly)) type_table in
    (VFunRec (f, x, e, labeled_env, symbol_env), symbol, formula, type_table)
  (* ?? *)
  | ELet (f, is_rec, args, typ, e1, e2) ->
    begin match args with
    | [] ->
      let (value1, symbol1, formula, type_table) = gen_exp_constraint labeled_env symbol_env formula e1 type_table in
      let value1 = 
        if is_rec then
          begin match value1 with
          | VFun (x, e, labeled_closure, symbolic_closure) -> VFunRec (f, x, e, labeled_closure, symbolic_closure)
          | _ -> value1 
          end
        else
          value1
      in
      let (value2, symbol2, formula, type_table) = gen_exp_constraint (extend_env (f, value1) labeled_env) (extend_env (f, symbol1) symbol_env) formula e2 type_table in
      let t = find_env symbol2 type_table in
      let formula = extend_set (symbol, Symbol symbol2) formula in
      let type_table = extend_env (symbol, t) type_table in
      (value2, symbol, formula, type_table)
    | _ ->
      let fn = 
      if is_rec then 
        let e1 = binding_to_funs (List.tl args) e1 in
        let (x1, t1) = List.hd args in
        let typ = types_to_arr ((List.map snd (List.tl args)) @ [typ]) in
        (-1, EFix (f, (x1, t1), typ, e1))
      else 
        binding_to_funs args e1 
      in
      let (value1, symbol1, formula, type_table) = gen_exp_constraint labeled_env symbol_env formula fn type_table in
      let (value2, symbol2, formula, type_table) = gen_exp_constraint (extend_env (f, value1) labeled_env) (extend_env (f, symbol1) symbol_env) formula e2 type_table in
      let t = find_env symbol2 type_table in
      let formula = extend_set (symbol, Symbol symbol2) formula in
      let type_table = extend_env (symbol, t) type_table in
      (value2, symbol, formula, type_table)
    end
  | EApp (e1, e2) ->
    let (value1, symbol1, formula, type_table) = gen_exp_constraint labeled_env symbol_env formula e1 type_table in
    let (value2, symbol2, formula, type_table) = gen_exp_constraint labeled_env symbol_env formula e2 type_table in
    begin match value1 with
    | VFun (x, e, labeled_closure, symbolic_closure) ->
      let (value3, symbol3, formula, type_table) = gen_exp_constraint (extend_env (x, value2) labeled_closure) (extend_env (x, symbol2) symbolic_closure) formula e type_table in
      let t = find_env symbol3 type_table in
      let formula = extend_set (symbol, Symbol symbol3) formula in
      let type_table = extend_env (symbol, t) type_table in
      (value3, symbol, formula, type_table)
    | VFunRec (f, x, e, labeled_closure, symbolic_closure) ->
      let labeled_env = extend_env (x, value2) labeled_closure in
      let symbol_env = extend_env (x, symbol2) symbolic_closure in
      let (value3, symbol3, formula, type_table) = gen_exp_constraint (extend_env (f, value1) labeled_env) (extend_env (f, symbol1) symbol_env) formula e type_table in
      let t = find_env symbol3 type_table in
      let formula = extend_set (symbol, Symbol symbol3) formula in
      let type_table = extend_env (symbol, t) type_table in
      (value3, symbol, formula, type_table)
    | _ -> 
      raise (Failure "Function type error")
    end
  | EMatch (e, bs) ->
    (* find true branch *)
    let (value1, symbol1, formula, type_table) = gen_exp_constraint labeled_env symbol_env formula e type_table in
    let (p, ex) = find_first_branch value1 bs in
    (* make a formula of pattern matching *)
    let (labeled_env, symbol_env, symbol2, formula, type_table) = gen_pat_constraint labeled_env symbol_env formula (value1, p) type_table in
    let symbol_match = fresh_symbol () in
    let formula = extend_set (symbol_match, Eq (Symbol symbol1, Symbol symbol2)) formula in
    let type_table = extend_env (symbol_match, TBool) type_table in
    (* constraint of body expression *)
    let (value', symbol', formula, type_table) = gen_exp_constraint labeled_env symbol_env formula ex type_table in
    let symbol4 = fresh_symbol() in
    let formula = extend_set (symbol4, Unknown) formula in
    let type_table = extend_env (symbol4, (find_env symbol' type_table)) type_table in
    (* constraint of match expression *)
    let symbol_value = If (Symbol symbol_match, Symbol symbol', Symbol symbol4) in
    let formula = extend_set (symbol, symbol_value) formula in
    let type_table = extend_env (symbol, (find_env symbol' type_table)) type_table in
    (value', symbol, formula, type_table)
  |_ -> raise (Failure ("gen_exp_constraint error" ^ Print.labeled_exp_to_string (l,exp)))

and gen_exp_list_constraint : labeled_env -> symbolic_env -> formula -> labeled_exp list -> symbol_type_table -> (labeled_value list * symbolic_value list * typ list * formula * symbol_type_table)
= fun labeled_env symbol_env formula exps type_table ->
  List.fold_left (
    fun (values, symbols, typs, formula, type_table) exp ->
      let (value, symbol, formula, type_table) = gen_exp_constraint labeled_env symbol_env formula exp type_table in
      (values@[value], symbols@[Symbol symbol], typs@[(find_env symbol type_table)], formula, type_table)
  ) ([], [], [], formula, type_table) exps

let rec gen_decl_constraint : labeled_env -> symbolic_env -> formula -> labeled_decl -> symbol_type_table -> (labeled_env * symbolic_env * formula * symbol_type_table)
= fun labeled_env symbol_env formula l_decl type_table ->
  let _ = flag:=0 in
  let _ = start_time:=Unix.gettimeofday()  in
  match l_decl with
  | DData (x, ctors) -> (labeled_env, symbol_env, formula, type_table)
  | DLet (f, is_rec, args, typ, exp) ->
    let dummy_exp = (-1, ELet (f, is_rec, args, typ, exp, (-1, EVar f))) in
    let (value, symbol, formula, type_table) = gen_exp_constraint labeled_env symbol_env formula dummy_exp type_table in
    ((extend_env (f, value) labeled_env), (extend_env (f, symbol) symbol_env), formula, type_table)

let rec gen_pgm_constraint : labeled_env -> symbolic_env -> formula -> labeled_prog -> symbol_type_table -> (labeled_env * symbolic_env * formula * symbol_type_table)
= fun labeled_env symbol_env formula l_decls type_table ->
  match l_decls with
  | [] -> (labeled_env, symbol_env, formula, type_table)
  | hd::tl ->
    let (labeled_env, symbol_env, formula, type_table) = gen_decl_constraint labeled_env symbol_env formula hd type_table in
    gen_pgm_constraint labeled_env symbol_env formula tl type_table

(*
  generate counter examples' constraint set for program
*)
let rec gen_constraint_set : labeled_prog -> example -> (formula * symbol_type_table)
= fun l_pgm (input,output) ->
  let result = labeling_exp (appify (EVar "f") input) in
  let (labeled_env,symbol_env,formula,type_table) = gen_pgm_constraint empty_env empty_env empty_set l_pgm empty_env in
  let (value,symbol,formula,type_table) = gen_exp_constraint labeled_env symbol_env formula result type_table in
  let (formula, type_table) = gen_value_constraint formula type_table symbol output in
  (formula,type_table)

(*
 let gen_candidate_exp : formula -> symbol_type_table ->(symbolic_variable * symbolic_value) -> symbolic_variable BatSet.t -> symbolic_variable BatSet.t
= fun formula type_table (sym_var,sym_val) var_set ->
  let label2symbol = !label_to_symbol in
  let symbol2label = !symbol_to_label in
  try
    let label = BatMap.find sym_var symbol2label in
    let symbol_set = BatMap.find label label2symbol in
    let test_set = BatSet.diff formula (BatSet.filter (fun (s_var,s_val) -> BatSet.mem s_var symbol_set) formula) in 
    (*let test_set = BatSet.remove (sym_var,sym_val) formula in *)
    if(sym_val = Unknown) then var_set
    else if(New_z3.run test_set type_table)
      then BatSet.add sym_var var_set 
    else var_set
  with | _ -> var_set
*)

(*
let gen_candidate_label : formula -> symbol_type_table -> label -> symbol_set -> label BatSet.t -> label BatSet.t
= fun formula type_table label symbol_set label_set ->
  try
    let test_set = BatSet.filter (fun (s_var,s_val) -> not (BatSet.mem s_var symbol_set)) formula in

  let _ = print_endline("-------") in
    let _ = print_formula (BatSet.diff formula test_set) type_table in
    let _ = print_endline("-------") in  
    if(New_z3.run test_set type_table) then BatSet.add label label_set else label_set
  with
  | _ -> label_set 

let update_map : label -> (label,rank) BatMap.t -> (label,rank) BatMap.t
= fun label map ->
  try
    let rank = BatMap.find label map in
    BatMap.add label (rank+1) map
  with
  | _ -> BatMap.add label 1 map
*)

(*
  Make Candidate programs for synthesizing
  input: program P , examples E
  output: A set of weighted candidate programs
*)
let rec find_counter_examples : prog -> examples -> examples -> examples
= fun pgm exl l ->
  match exl with
  | [] -> l
  | hd::tl ->
    if (is_solution pgm [hd]) 
      then find_counter_examples pgm tl l
    else find_counter_examples pgm tl (hd::l)

let gen_counter_label : labeled_prog -> example -> label BatSet.t -> label BatSet.t
= fun l_pgm example set->
  (* Dymanic Symbolic Execution *)
  let (formula,type_table) = gen_constraint_set l_pgm example in
  !label_set

let gen_candidate_pgm : prog -> examples -> (int * prog) BatSet.t
= fun pgm examples ->
  let rank = cost pgm in
  let l_pgm = Labeling.labeling_prog pgm in
  let counter_examples = find_counter_examples pgm examples [] in
  let label_set = list_fold (gen_counter_label l_pgm) counter_examples BatSet.empty in
  let candidate_set = BatSet.fold (
    fun label set ->
      let hole_pgm = gen_hole_pgm l_pgm label in
      let candidate_pgm = unlabeling_prog hole_pgm in
      let rank' = cost candidate_pgm in
      if (is_closed candidate_pgm) then set else BatSet.add (rank-rank', candidate_pgm) set
  ) label_set BatSet.empty 
  in
  (*let _ = print_endline("-------------candidate programs--------") in
  let _ = BatSet.iter (
    fun (n,prog) -> 
      print_endline(string_of_int n);
      Print.print_pgm (prog);
      Print.print_pgm (translate prog)
  ) candidate_set 
  in*)
  (*let _ = print_endline ("Num of Candidates : " ^ string_of_int (BatSet.cardinal candidate_set)) in
 *) candidate_set

(*
  SAT Pruning
*)
let val2var : symbolic_value -> symbolic_variable
= fun s ->
  match s with
  | Symbol n -> n
  | _ -> raise (Failure "Not symbol error")

let rec update_type_table : formula -> symbol_type_table -> symbol_type_table
= fun formula type_table ->
  if (BatSet.is_empty formula) then type_table
  else
  try
    let ((sym_var, sym_val), remain) = BatSet.pop formula in
    let t = find_env sym_var type_table in
    if (is_poly t) then update_type_table remain type_table 
    else match (sym_val, t) with
      | (Int n, TInt)-> update_type_table remain type_table 
      | (Bool b, TBool) -> update_type_table remain type_table 
      | (Symbol n, t) -> 
        let type_table = extend_env (n, t) type_table in
        update_type_table remain type_table
      | (List ss, TList t) -> update_type_table remain type_table 
      (*
      | (Tuple ss, TTuple ts) ->
      | Ctor of id * symbolic_value list
      *)
      | (Add (s1, s2), TInt) -> 
        let type_table = extend_env ((val2var s1), TInt) type_table in
        let type_table = extend_env ((val2var s2), TInt) type_table in
        update_type_table remain type_table                          
      | (Sub (s1, s2), TInt) -> 
        let type_table = extend_env ((val2var s1), TInt) type_table in
        let type_table = extend_env ((val2var s2), TInt) type_table in
        update_type_table remain type_table                           
      | (Mul (s1, s2), TInt) -> 
        let type_table = extend_env ((val2var s1), TInt) type_table in
        let type_table = extend_env ((val2var s2), TInt) type_table in
        update_type_table remain type_table                                      
      | (Div (s1, s2), TInt) -> 
        let type_table = extend_env ((val2var s1), TInt) type_table in
        let type_table = extend_env ((val2var s2), TInt) type_table in
        update_type_table remain type_table                                        
      | (Mod (s1, s2), TInt) -> 
        let type_table = extend_env ((val2var s1), TInt) type_table in
        let type_table = extend_env ((val2var s2), TInt) type_table in
        update_type_table remain type_table                                        
      | (Minus s, TInt) ->
        let type_table = extend_env ((val2var s), TInt) type_table in
        update_type_table remain type_table        
      | (Not s, TBool) -> 
        let type_table = extend_env ((val2var s), TBool) type_table in
        update_type_table remain type_table                                
      | (Or (s1, s2), TBool) ->  
        let type_table = extend_env ((val2var s1), TBool) type_table in
        let type_table = extend_env ((val2var s2), TBool) type_table in
        update_type_table remain type_table                               
      | (And (s1, s2), TBool) ->   
        let type_table = extend_env ((val2var s1), TBool) type_table in
        let type_table = extend_env ((val2var s2), TBool) type_table in
        update_type_table remain type_table                               
      | (Lt (s1, s2), TBool) -> 
        let type_table = extend_env ((val2var s1), TInt) type_table in
        let type_table = extend_env ((val2var s2), TInt) type_table in
        update_type_table remain type_table                                 
      | (Gt (s1, s2), TBool) -> 
        let type_table = extend_env ((val2var s1), TInt) type_table in
        let type_table = extend_env ((val2var s2), TInt) type_table in
        update_type_table remain type_table                         
      | (Le (s1, s2), TBool) ->     
        let type_table = extend_env ((val2var s1), TInt) type_table in
        let type_table = extend_env ((val2var s2), TInt) type_table in
        update_type_table remain type_table               
      | (Ge (s1, s2), TBool) ->   
        let type_table = extend_env ((val2var s1), TInt) type_table in
        let type_table = extend_env ((val2var s2), TInt) type_table in
        update_type_table remain type_table                     
      | (Eq (s1, s2), _) ->   
        let s1 = val2var s1 in
        let s2 = val2var s2 in
        let t1 = find_env s1 type_table in
        let t2 = find_env s2 type_table in
        let t = type_of_bop t1 t2 in   
        let type_table = extend_env (s1, t) type_table in
        let type_table = extend_env (s2, t) type_table in
        update_type_table remain type_table                          
      | (NEq (s1, s2), _) ->        
        let s1 = val2var s1 in
        let s2 = val2var s2 in
        let t1 = find_env s1 type_table in
        let t2 = find_env s2 type_table in
        let t = type_of_bop t1 t2 in   
        let type_table = extend_env (s1, t) type_table in
        let type_table = extend_env (s2, t) type_table in
        update_type_table remain type_table                            
      | (At (s1, s2), TList t) ->
        let type_table = extend_env ((val2var s1), TList t) type_table in
        let type_table = extend_env ((val2var s2), TList t) type_table in
        update_type_table remain type_table   
      | (Cons (s1, s2), TList t) ->
        let type_table = extend_env ((val2var s1), t) type_table in
        let type_table = extend_env ((val2var s2), TList t) type_table in
        update_type_table remain type_table   
      | (If (s1, s2, s3), _) ->
        let type_table = extend_env ((val2var s1), TBool) type_table in
        let type_table = extend_env ((val2var s2), t) type_table in
        let type_table = extend_env ((val2var s3), t) type_table in
        update_type_table remain type_table   
      | (Unknown, _) -> update_type_table remain type_table 
      | _ -> raise (Failure "type error in type_table_update")
  with _ -> raise (Failure "not_found error in type_table_update")
(*
let check_sat : labeled_prog -> example -> bool
= fun l_pgm example ->
  let (formula,type_table) = gen_constraint_set l_pgm example in
  let formula = BatSet.filter(
    fun (sym_var, sym_val) ->
      match find_env sym_var type_table with
      |TArr (_,_) -> false
      |_ -> true
  ) formula 
  in
  let type_table = BatMap.filterv(
    fun typ ->
      match typ with
      |TArr (_,_) -> false
      |_ -> true
  ) type_table 
  in
  let type_table = fix (update_type_table formula) type_table in
  (*let _ = Print.print_examples [example] in
  let _ = Print.print_formula formula type_table in
  *)
  New_z3.run formula type_table

let sat_time = ref 0.0
let result_sat = ref 0.0

  
let sat_pruning : prog -> examples -> bool
= fun pgm examples ->
  let _ = sat_time:=Unix.gettimeofday() in
  let pgm = translate pgm in
  let _ = pgm_string:= Print.program_to_string pgm in
  (*let _ = Print.print_pgm pgm in
  *)let key = Print.program_to_string pgm in
  try 
    BatMap.find key !store
  with _ ->
    let l_pgm = Labeling.labeling_prog pgm in
    let t = List.for_all (check_sat l_pgm) examples in
    store := Memoization.add key t !store;
    let _ = sat_time:=Unix.gettimeofday() -. !sat_time in
    let _ = result_sat := !result_sat+. !sat_time in
    t
*)
let localization : prog -> examples -> (int * prog) BatSet.t
= fun pgm examples ->
  let candidate_set = gen_candidate_pgm pgm examples in
  let result_set = (*BatSet.filter (
    fun (rank, pgm) ->
      (*let _ = Print.print_pgm (translate pgm) in*)
      sat_pruning pgm examples
  ) candidate_set 
  in
  (* Debuging *)
  let _ = print_endline("-------------Pruned programs--------") in
  let _ = BatSet.iter (
    fun (n,prog) -> 
      print_endline(string_of_int n);
      Print.print_pgm (prog);
  ) (BatSet.diff candidate_set result_set) 
  in
  let _ = print_endline("-------------Candidate programs--------") in
  let _ = BatSet.iter (
    fun (n,prog) -> 
      print_endline(string_of_int n);
      Print.print_pgm (prog);
  ) result_set
  in
  let _ = print_endline ("Num of Candidates : " ^ string_of_int (BatSet.cardinal result_set)) in
  let _ = print_endline ("Num of Pruned Candidates : " ^ string_of_int (BatSet.cardinal (BatSet.diff candidate_set result_set))) in*)
  candidate_set in
  result_set
