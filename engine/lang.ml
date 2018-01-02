open Util

exception Eval_error of string

type id = string 
type typ = 
  | TInt
  | TBool
  | TPoly
  | TString
  | TBase of id  (* user defined*)
  | TList of typ
  | TTuple of typ list
  | TCtor of typ * typ list (*  tbase x , tl *)
  | TArr of typ * typ (*fun t1->t2->t3...*)
  | TVar of id (* type variable *)

type ctor = id * typ list           (* C t1 .. tn *)

type pat = PCtor of id * pat list (*pval*) 
          | Pats of pat list
          | PInt of int
          | PVar of id
          | PBool of bool
          | PList of pat list  (*pval*)
          | PTuple of pat list  (*pval*)
          | PUnder
          | PCons of pat list  (*pval*)
          | PHole of int
          (*| pvalue *)

type arg = id * typ

and decl =
  | DData of id * ctor list                         (* 'type' D = ctors *)
  | DLet  of id * bool * arg list * typ * exp       (* let x [rec] (x1:t1) .. (xn:tn) : t = e *)

and exp =
  (* aexp *)
  | Const of int
  | String of id
  | ADD of exp * exp                                (*a1 + a2*)
  | SUB of exp * exp                                (*a1 - a2*)
  | MUL of exp * exp                                (*a1 * a2*)
  | DIV of exp * exp                                (*a1 / a2*)
  | MOD of exp * exp                                (*a1 % a2*)
  | MINUS of exp
  (* bexp *)
  | TRUE
  | FALSE
  | NOT of exp                                      (*not b1*)
  | OR of exp * exp                                 (*b1 || b2*)
  | AND of exp * exp                                (*b1 && b2*)
  | LESS of exp * exp                               (*a1 < a2*)
  | LARGER of exp * exp                             (*a1 > a2*)
  | EQUAL of exp * exp                              (*a1 == a2*)
  | NOTEQ of exp * exp                              (*a1 <> a2 or a1 != a2*)
  | LESSEQ of exp * exp                             (*a1 <= a2*)
  | LARGEREQ of exp * exp                           (*a1 >= a2*)
  (* else *)
  | EVar of id                                      (* x *)
  | EApp of exp * exp                               (* e1 e2 *)
  | EFun of arg * exp                               (* fun (x:t1) -> e *)
  (**)
  | ELet of id * bool * arg list * typ * exp * exp  (* let [rec] (x1:t1) .. (xn:tn) : t = e1 in e2 *)
  | ECtor of id * exp list                          (* C (e1, .., en) *)
  | EMatch of exp * branch list                     (* match e with bs *)
  | EPFun of (exp * exp) list                       (* v11 => v21 | .. | v1n => v2n *)
  | EFix  of id * arg * typ * exp                   (* fix f (x:t1) : t2 = e *)
  (**)
  | IF of exp * exp * exp
  (*List operation*)
  | AT of exp * exp
  | DOUBLECOLON of exp * exp
  | EList of exp list
  | ETuple of exp list
  | Hole of int
and branch = pat * exp   

type prog = decl list

(* semantics *)
type value =
  | VInt of int
  | VString of string
  | VBool of bool
  | VList of value list (* ?? *)
  | VTuple of value list
  | VCtor of id * value list
  | VFun  of id * exp * env
  | VFunRec of id * id * exp * env
  | VPFun of (value * value) list
  | VHole of int

and env = (id, value) BatMap.t
and components = exp BatSet.t * pat BatSet.t

type example = (exp list * value)
type examples = (exp list* value) list

let rec exp_cost : exp -> int 
= fun exp ->
  match exp with
  | Const n -> 7
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
  | MINUS e1 -> 15 + (exp_cost e1)
  | NOT e1 -> 15 + (exp_cost e1)
  | EVar x -> 7
  | EApp (e1,e2) -> 10 + (exp_cost e1) + (exp_cost e2)
  | ELet (x,is_rec,args,typ,e1,e2) -> (if (is_rec) then 50 else 40) + (exp_cost e1) + (exp_cost e2)
  | ECtor (x,l) -> 
    let rec f lst =
    match lst with
    [] -> 0
    |hd::tl -> (exp_cost hd) + (f tl) 
  in 10+ (f l) 
  | EMatch (e1,bl) -> 
    let (pl,el) = List.split bl in
    let rec f_pat lst = 
    match lst with 
    [] -> 0
    |hd::tl -> (pat_cost hd) + (f_pat tl)
  in let rec f_exp lst =
    match lst with
    [] -> 0
    |hd::tl -> (exp_cost hd) + (f_exp tl)
  in 40+(f_pat pl)+(f_exp el)+(exp_cost e1)
  | EFun (arg,e) -> 40 + (exp_cost e)
  | IF (e1,e2,e3) -> 40 + (exp_cost e1) + (exp_cost e2) + (exp_cost e3)
  | AT (e1,e2) -> 15 + (exp_cost e1) + (exp_cost e2)
  | DOUBLECOLON (e1,e2) -> 15 + (exp_cost e1) + (exp_cost e2)
  | EList l -> 
    let rec f lst =
    match lst with
    [] -> 0
    |hd::tl -> (exp_cost hd) + (f tl) 
  in 20+ (f l) 
  | ETuple l-> 
    let rec f lst =
    match lst with
    [] -> 0
    |hd::tl -> (exp_cost hd) + (f tl) 
  in 20+ (f l) 
  | Hole n-> 15
  | _ -> 0

and pat_cost : pat -> int
= fun pat ->
    match pat with
    PCtor (x,lst) -> 
      let rec f l = 
      match l with
      [] -> 0
      |hd::tl -> (pat_cost hd) + (f tl)
    in 10 + f lst
    | Pats lst -> 
      let rec f l = 
      match l with
      [] -> 0
      |hd::tl -> (pat_cost hd) + (f tl)
    in 50 + f lst
    | PInt _ -> 30
    | PVar _ -> 15
    | PBool b -> 30
    | PList lst ->  
      let rec f l = 
      match l with
      [] -> 0
      |hd::tl -> (pat_cost hd) + (f tl)
    in 30 + f lst
    | PTuple lst -> 
      let rec f l = 
      match l with
      [] -> 0
      |hd::tl -> (pat_cost hd) + (f tl)
    in 30 + f lst  
    | PUnder -> 20
    | PCons lst -> 
      let rec f l = 
      match l with
      [] -> 0
      |hd::tl -> (pat_cost hd) + (f tl)
    in 20 + f lst
    | PHole _ -> 50


let cost_decl : decl -> int -> int
= fun decl cost ->
  match decl with
  | DData _ -> cost
  | DLet (x,is_rec,args,typ,exp) -> 
    match args with
    | [] -> (* variable binding *)
      cost+(exp_cost exp) 
    | _ ->  (* function binding *)
      cost+(exp_cost (ELet (x,is_rec,args,typ,exp, EVar x)))

let cost : prog -> int
= fun decls ->  list_fold cost_decl decls 0

let exp_hole_count = ref 0
let gen_hole : unit -> exp
= fun () -> exp_hole_count:=!exp_hole_count+1; Hole(!exp_hole_count)

let pat_hole_count = ref 0
let gen_pat_hole : unit -> pat
= fun () -> pat_hole_count:=!pat_hole_count+1; PHole(!pat_hole_count)

let empty_env = BatMap.empty
let lookup_env = BatMap.find
let update_env = BatMap.add
let update_env_a ids vals env = 
  list_fold (fun (x,v) -> update_env x v) (List.combine ids vals) env

let rec appify : exp -> exp list -> exp
= fun exp exp_list ->
	match exp_list with
	[] -> exp
	|hd::tl -> appify (EApp(exp,hd)) tl
