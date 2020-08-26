open Util

exception EvalError of string
exception TimeoutError
exception StackOverflow of string
exception EqualError

type id = string 

type label = int

(* Type *)
type typ = 
  | TUnit
  | TInt
  | TBool
  | TString
  | TBase of id  (* user defined*)
  | TList of typ
  | TTuple of typ list
  | TCtor of typ * typ list (*  tbase x , tl *)
  | TArr of typ * typ (*fun t1->t2->t3...*)
  | TVar of id (* type variable *)
  | TRef of typ
  | TExn

type ctor = id * typ list
(* Pattern *)
type pat = 
  | PUnit
  | PUnder 
  | PInt of int
  | PBool of bool
  | PVar of id
  | PList of pat list
  | PCons of pat * pat
  | PTuple of pat list
  | Pats of pat list
  | PCtor of id * pat list

(* Program *)
type let_bind =
  | BindUnder (* let _ = ... in x *)
  | BindOne of id (* let x = ... in x *)
  | BindTuple of let_bind list (* let x,y = (..., ...) in x,y *)

type arg = 
  | ArgUnder of typ
  | ArgOne of id * typ
  | ArgTuple of arg list

type decl =
  | DExcept of ctor                                 (* exception x of t *)
  | DEqn of id * typ 
  | DData of id * ctor list                         (* 'type' D = ctors *)
  | DLet  of binding       (* let x [rec] (x1:t1) .. (xn:tn) : t = e *)
  (* and block *)
  | DBlock of bool * binding list (* let x1 = e1 and x2 = e2 ... xn = en | let rec f1 x1 = e1 and f2 x2 = e2 and ... fn xn = en *)
  | TBlock of decl list
and lexp = label * exp											 
and exp =
  (* Const *)
  | EUnit
  | Const of int
  | TRUE
  | FALSE  
  | EList of lexp list
  | String of id
  | EVar of id         
  | ECtor of id * lexp list
  | ETuple of lexp list                             
  (* aop *)
  | ADD of lexp * lexp                                (*a1 + a2*)
  | SUB of lexp * lexp                                (*a1 - a2*)
  | MUL of lexp * lexp                                (*a1 * a2*)
  | DIV of lexp * lexp                                (*a1 / a2*)
  | MOD of lexp * lexp                                (*a1 % a2*)
  | MINUS of lexp
  (* bop *)
  | NOT of lexp                                      (*not b1*)
  | OR of lexp * lexp                                 (*b1 || b2*)
  | AND of lexp * lexp                                (*b1 && b2*)
  | LESS of lexp * lexp                               (*a1 < a2*)
  | LARGER of lexp * lexp                             (*a1 > a2*)
  | EQUAL of lexp * lexp                              (*a1 == a2*)
  | NOTEQ of lexp * lexp                              (*a1 <> a2 or a1 != a2*)
  | LESSEQ of lexp * lexp                             (*a1 <= a2*)
  | LARGEREQ of lexp * lexp                           (*a1 >= a2*)
  (* lop *)
  | AT of lexp * lexp
  | DOUBLECOLON of lexp * lexp
  | STRCON of lexp * lexp
  (* else *)
  | EApp of lexp * lexp                              (* e1 e2 *)
  | EFun of arg * lexp                               (* fun (x:t1) -> e *)
  | ELet of let_bind * bool * arg list * typ * lexp * lexp  (* let [rec] (x1:t1) .. (xn:tn) : t = e1 in e2 *)
  | EBlock of bool * binding list * lexp (* let x1 = e1 and x2 = e2 and ... xn = en in e' | let rec f1 x1 = e1 and f2 x2 = e2 ... fn xn = en in e' *)
  | EMatch of lexp * branch list                     (* match e with bs *)
  | IF of lexp * lexp * lexp
  (* Exception *)
  | Raise of lexp
  (* Reference *)
  | ERef of lexp (* ref e *)
  | EDref of lexp (* !e *)
  | EAssign of lexp * lexp (* e1 := e2 *)
  (* Special *)
  | Hole of int
  | SInt of int 
  | SStr of int
and branch = pat * lexp   
and binding = (let_bind * bool * arg list * typ * lexp) (* f [rec] x1,x2 :t = e => must divide LET & LETREC later *)

type prog = decl list

(* values *)
type loc = int 
type value =
  | VUnit
  | VInt of int
  | VString of string
  | VBool of bool
  | VList of value list
  | VTuple of value list
  | VCtor of id * value list
  | VFun  of arg * lexp * env
  | VFunRec of id * arg * lexp * env
  | VBlock of id * (id * value) list
  | VRef of loc
  | VHole of int
(* environemnt *)
and env = (id, value) BatMap.t

let empty_env = BatMap.empty
let lookup_env = BatMap.find
let update_env = BatMap.add
(* memory *)
type mem = (loc, value) BatMap.t

let empty_mem = BatMap.empty
let lookup_mem = BatMap.find
let update_mem = BatMap.add

let loc_count = ref 0
let fresh_loc () = loc_count:=!loc_count+1; !loc_count

(* exception *)
exception EExcept of value

(* others *)
type input = lexp list
type example = (input * value)
type examples = (input * value) list
type components = lexp BatSet.t

let const_count = ref 0
let gen_const : unit -> int
= fun () -> const_count:=!const_count+1; !const_count

(* Label *)
let label_count = ref 0
let gen_label : unit -> label
= fun () -> label_count:=!label_count+1; (!label_count)
let get_label : lexp -> label
= fun (l, e) -> l

let rec get_labels_exp : lexp -> label BatSet.t
= fun (l, exp) ->
  let sub_labels = 
    match exp with
    | EList es | ECtor (_, es) | ETuple es -> 
      List.fold_left (fun labels e -> BatSet.union labels (get_labels_exp e)) BatSet.empty es
    | EFun (_, e) | MINUS e | NOT e | Raise e | ERef e | EDref e -> BatSet.union (get_labels_exp e) (BatSet.singleton l)
    | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) 
    | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LARGER (e1, e2) | LESSEQ (e1, e2) | LARGEREQ (e1, e2) 
    | EQUAL (e1, e2) | NOTEQ (e1, e2) | AT (e1, e2) | DOUBLECOLON (e1, e2) | STRCON (e1, e2) | EApp (e1, e2) 
    | EAssign (e1, e2) | ELet (_, _, _, _, e1, e2) ->
      BatSet.singleton l
      |> BatSet.union (get_labels_exp e1)
      |> BatSet.union (get_labels_exp e2)
    | EBlock (_, ds, e) ->
      let es = e::(List.map (fun (_, _, _, _, e) -> e) ds) in
      List.fold_left (fun labels e -> BatSet.union labels (get_labels_exp e)) BatSet.empty es
    | EMatch (e, bs) ->
      List.fold_left (fun labels (p, e) -> BatSet.union labels (get_labels_exp e)) (get_labels_exp e) bs
    | IF (e1, e2, e3) ->
      BatSet.singleton l
      |> BatSet.union (get_labels_exp e1)
      |> BatSet.union (get_labels_exp e2)
      |> BatSet.union (get_labels_exp e3)
    | _ -> BatSet.empty
  in
  BatSet.add l sub_labels

let get_labels_decl : decl -> label BatSet.t 
= fun decl ->
  match decl with
  | DLet (_, _, _, _, e) -> get_labels_exp e
  | DBlock (_, ds) -> List.fold_left (fun labels (_, _, _, _, e) -> BatSet.union labels (get_labels_exp e)) BatSet.empty ds 
  | _ -> BatSet.empty

let get_labels : prog -> label BatSet.t
= fun pgm -> List.fold_left (fun labels decl -> BatSet.union labels (get_labels_decl decl)) BatSet.empty pgm

(* Hole *)
let hole_count = ref 0
let gen_hole : unit -> exp
= fun () -> hole_count:=!hole_count+1; Hole(!hole_count)

let gen_labeled_hole : unit -> lexp
= fun () -> (gen_label(), gen_hole())

let dummy_hole () = (0, Hole 0)

(* Function hole *)
let fhole_count = ref 0
let gen_fhole () = fhole_count := !fhole_count - 1; Hole (!fhole_count)
let gen_labeled_fhole () = (gen_label (), gen_fhole ())

(* External functions *)
let library_pgm : prog ref = ref []
let grading_pgm : prog ref = ref []

(* generate a fresh type variable *)
let tvar_num = ref 0
let fresh_tvar () = (tvar_num := !tvar_num + 1; (TVar ("#" ^ string_of_int !tvar_num)))

let rec is_fun : typ -> bool
= fun typ ->
  match typ with
  | TArr _ -> true
  | TList typ -> is_fun typ
  | TTuple ts -> List.exists is_fun ts
  | TCtor (tname, ts) -> (is_fun tname) || (List.exists is_fun ts)
  | _ -> false

let rec get_func_typ : arg list -> typ -> typ
= fun args typ ->
  let rec arg_to_typ arg =
    match arg with
    | ArgUnder typ | ArgOne (_, typ) -> typ 
    | ArgTuple args -> TTuple (List.map arg_to_typ args)
  in
  List.fold_left (fun acc arg -> TArr (arg_to_typ arg, acc)) typ (List.rev args)

let rec get_output_typ : typ -> typ
= fun typ ->
  match typ with
  | TArr (t1, t2) -> get_output_typ t2
  | _ -> typ 

(* function application *)
let rec appify : lexp -> lexp list -> lexp
= fun exp exp_list ->
	match exp_list with
	[] -> exp
	|hd::tl -> appify (gen_label(),EApp(exp,hd)) tl

let rec let_to_exp : let_bind -> lexp
= fun x ->
  match x with 
  | BindOne x -> (gen_label(),EVar x)
  | BindTuple xs -> (gen_label(),ETuple (List.map let_to_exp xs))
  | _ -> raise (Failure "Wild-card _ is not valid")

(* Expression updating function *)
let update_unary : exp -> lexp -> exp
= fun exp e ->
  match exp with
  | Raise _ -> Raise e 
  | MINUS _ -> MINUS e  
  | NOT _ -> NOT e
  | ERef _ -> ERef e
  | EDref _ -> EDref e
  | _ -> raise (Failure "Updating of unary expression is failed")

let update_binary : exp -> lexp * lexp -> exp
= fun exp (e1, e2) ->
  match exp with 
  | ADD _ -> ADD (e1, e2)
  | SUB _ -> SUB (e1, e2)
  | MUL _ -> MUL (e1, e2)
  | DIV _ -> DIV (e1, e2)
  | MOD _ -> MOD (e1, e2)
  | OR _ -> OR (e1, e2)
  | AND _ -> AND (e1, e2)
  | LESS _ -> LESS (e1, e2)
  | LESSEQ _ -> LESSEQ (e1, e2)
  | LARGER _ -> LARGER (e1, e2)
  | LARGEREQ _ -> LARGEREQ (e1, e2)
  | EQUAL _ -> EQUAL (e1, e2)
  | NOTEQ _ -> NOTEQ (e1, e2)
  | DOUBLECOLON _ -> DOUBLECOLON (e1, e2)
  | AT _ -> AT (e1, e2)
  | STRCON _ -> STRCON (e1, e2)
  | EApp _ -> EApp (e1, e2)
  | EAssign _ -> EAssign (e1, e2)
  | _ -> raise (Failure "Updating of binary expression is failed")

(* cost function based-on Occam's razor *)
let rec exp_cost : lexp -> int 
= fun (_,exp) ->
  match exp with
  | SInt _ -> 15
  | SStr _ -> 15
  | EUnit -> 50
  | Const n -> 15
  | TRUE -> 15
  | FALSE -> 15
  | String x -> 15
  | ADD (e1,e2) -> 15 + (exp_cost e1) + (exp_cost e2)
  | SUB (e1,e2) -> 15 + (exp_cost e1) + (exp_cost e2)
  | MUL (e1,e2) -> 15 + (exp_cost e1) + (exp_cost e2)
  | DIV (e1,e2) -> 15 + (exp_cost e1) + (exp_cost e2)
  | MOD (e1,e2) -> 15 + (exp_cost e1) + (exp_cost e2)
  | OR (e1,e2) -> 15 + (exp_cost e1) + (exp_cost e2)
  | AND (e1,e2) -> 15 + (exp_cost e1) + (exp_cost e2)
  | LESS (e1,e2) -> 15 + (exp_cost e1) + (exp_cost e2)
  | LARGER (e1,e2) -> 15 + (exp_cost e1) + (exp_cost e2)
  | EQUAL (e1,e2) -> 15 + (exp_cost e1) + (exp_cost e2)
  | NOTEQ (e1,e2) -> 15 + (exp_cost e1) + (exp_cost e2)
  | LESSEQ (e1,e2) -> 15 + (exp_cost e1) + (exp_cost e2)
  | LARGEREQ (e1,e2) -> 15 + (exp_cost e1) + (exp_cost e2)
  | STRCON (e1,e2) -> 15 + (exp_cost e1) + (exp_cost e2)
  | MINUS e1 -> 15 + (exp_cost e1)
  | NOT e1 -> 15 + (exp_cost e1)
  | EVar x -> 10
  | EApp (e1,e2) -> 10 + (exp_cost e1) + (exp_cost e2)
  | ELet (x,is_rec,args,typ,e1,e2) -> (if (is_rec) then 50 else 40) + (exp_cost e1) + (exp_cost e2)
  | EBlock (is_rec, es, e2) -> 100 + List.fold_left (fun acc (x, is_rec, args, typ, e) -> acc+(exp_cost e)) 0 es
  | ECtor (x,l) -> 40 + (list_fold(fun e r -> exp_cost e + r) l 0)
  | EMatch (e1,bl) -> 
    let (pl,el) = list_split bl in
    10 + (exp_cost e1)+(list_fold(fun p r -> pat_cost p+r) pl 0) + (list_fold(fun e r ->exp_cost e+r) el 0)
  | EFun (arg,e) -> 30 + (exp_cost e)
  | IF (e1,e2,e3) -> 10 + (exp_cost e1) + (exp_cost e2) + (exp_cost e3)
  | AT (e1,e2) -> 15 + (exp_cost e1) + (exp_cost e2)
  | DOUBLECOLON (e1,e2) -> 15 + (exp_cost e1) + (exp_cost e2)
  | EList l -> 10 + (list_fold (fun e r -> exp_cost e + r) l 0)
  | ETuple l-> 5 + (list_fold (fun e r -> exp_cost e + r) l 0) 
  | Hole n-> 23
  | Raise e -> 30 + (exp_cost e) 
  | _ -> 100
  
and pat_cost : pat -> int
= fun pat ->
    match pat with
    PCtor (x,lst) -> 10 + (list_fold (fun p r -> pat_cost p+r) lst 0) 
  | Pats lst -> 10 + (list_fold (fun p r -> pat_cost p+r) lst 0)
  | PUnit -> 30
  | PInt _ -> 10
  | PVar _ -> 10
  | PBool b -> 10
  | PList lst | PTuple lst -> 10 + (list_fold (fun p r -> pat_cost p+r) lst 0)  
  | PUnder -> 10
  | PCons (p1, p2) -> 10 + pat_cost p1 + pat_cost p2

let cost_decl : decl -> int -> int
= fun decl cost ->
  match decl with
  | DLet (x,is_rec,args,typ,exp) -> 
    begin match args with
    | [] -> (* variable binding *)
      cost+(exp_cost exp) 
    | _ ->  (* function binding *)
      cost+(exp_cost (gen_label() ,ELet (x,is_rec,args,typ,exp, let_to_exp x)))
    end
	| DBlock (_,bindings) ->
		list_fold (fun (_,_,_,_,e) r -> exp_cost e + r) bindings cost
  | _ -> cost

let cost : prog -> int
= fun decls -> list_fold cost_decl decls 0

(* Size of AST *)
let rec exp_size : lexp -> int 
= fun (l, exp) ->
  match exp with
  | EUnit | Const _ | TRUE | FALSE | String _ | EVar _ -> 1
  | EList es | ETuple es | ECtor (_, es) -> 1 + List.fold_left (fun acc e -> acc + exp_size e) 0 es
  | ERef e | EDref e | Raise e | MINUS e | NOT e | EFun (_, e) -> 1 + exp_size e
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2)
  | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LESSEQ (e1, e2) | LARGER (e1, e2) | LARGEREQ (e1, e2) 
  | EQUAL (e1, e2) | NOTEQ (e1, e2) | DOUBLECOLON (e1, e2) | AT (e1, e2) | STRCON (e1, e2)
  | EApp (e1, e2) | EAssign (e1, e2) | ELet (_, _, _, _, e1, e2) -> 1 + exp_size e1 + exp_size e2
  | EBlock (_, ds, e2) -> 1 + exp_size e2 + List.fold_left (fun acc (f, is_rec, args, typ, e) -> acc + exp_size e) 0 ds
  | EMatch (e, bs) -> 1 + exp_size e + List.fold_left (fun acc (p, e) -> acc + exp_size e) 0 bs
  | IF (e1, e2, e3) -> 1 + exp_size e1 + exp_size e2 + exp_size e3
  | Hole _ | SInt _ | SStr _ -> 1

(* Component update *)
let rec update_component : lexp -> lexp
= fun (l, exp)->
  let l = gen_label () in
  match exp with
  | EUnit | Const _ | TRUE | FALSE | String _ | EVar _ -> (l, exp)
  | EFun (arg, e) -> (gen_label (), EFun (arg, update_component e))
  | ERef e | EDref e | Raise e | MINUS e | NOT e -> (l, update_unary exp (update_component e))
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) 
  | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LESSEQ (e1, e2) | LARGER (e1, e2) | LARGEREQ (e1, e2) 
  | EQUAL (e1, e2) | NOTEQ (e1, e2) | DOUBLECOLON (e1, e2) | AT (e1, e2) | STRCON (e1, e2) | EApp (e1, e2) 
  | EAssign (e1, e2) -> (l, update_binary exp (update_component e1, update_component e2))
  | EList es -> (l, EList (List.map (fun e -> update_component e) es))
  | ETuple es -> (l, ETuple (List.map (fun e -> update_component e) es))
  | ECtor (x, es) -> (l, ECtor (x, List.map (fun e -> update_component e) es))
  | IF (e1, e2, e3) -> (l, IF (update_component e1, update_component e2, update_component e3))
  | EMatch (e, bs) -> 
    let bs = List.map (fun (p, e) -> (p, update_component e)) bs in
    (l, EMatch (update_component e, bs))
  | ELet (f, is_rec, args, typ, e1, e2) -> (l, ELet (f, is_rec, args, typ, update_component e1, update_component e2))
  | EBlock (is_rec, bindings, e2) -> 
    let ds = List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, typ, update_component e)) bindings in
    (l, EBlock (is_rec, ds, update_component e2))
  | Hole _ -> (l, gen_hole ())
  | SInt _ | SStr _ -> (l, exp)