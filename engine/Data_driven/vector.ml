open Lang
open Util
open Type

exception NotImplemented
exception Invalid_Expression_Construct

module N = struct
    (*ast flattening*)
    type lnode = label * node
    and node = 
    | Empty | Id of id  | TUnit | TInt | TBool | TString
    | TBase of node  (* user defined*)
    | TList of node 
    | TTuple of node list
    | TCtor of node * node list (*  tbase x , tl *)
    | TArr of node * node (*fun t1->t2->t3...*)
    | TVar of node (* type variable *)
    | TExn
    | Ctor of node * node list (*ctor*)
    | PUnit | PUnder | PInt of int | PBool of bool | PVar of node
    | PList of node list | PCons of node list | PTuple of node list
    | Pats of node list | PCtor of node * node list
    | BindUnder | BindOne of node | BindTuple of node list
    | ArgUnder of node | ArgOne of node * node 
    | ArgTuple of node list
    | Binding of (node * bool * node list * node * lnode) (*binding*)
    | DExcept of node (*node = ctor*)
    | DEqn of node * node
    | DData of node * node list (*node = ctor*)
    | DLet of node (*node = binding*)
    | DBlock of bool * node list (*node = binding*)
    | TBlock of node list
    (* Const *)
    | EUnit | Const of int | TRUE | FALSE  
    | EList of lnode list | String of node
    | EVar of node | ECtor of node * lnode list
    | ETuple of lnode list                             
    (* aop *)
    | ADD of lnode * lnode                                (*a1 + a2*)
    | SUB of lnode * lnode                                (*a1 - a2*)
    | MUL of lnode * lnode                                (*a1 * a2*)
    | DIV of lnode * lnode                                (*a1 / a2*)
    | MOD of lnode * lnode                                (*a1 % a2*)
    | MINUS of lnode
    (* bop *)
    | NOT of lnode                                        (*not b1*)
    | OR of lnode * lnode                                 (*b1 || b2*)
    | AND of lnode * lnode                                (*b1 && b2*)
    | LESS of lnode * lnode                               (*a1 < a2*)
    | LARGER of lnode * lnode                             (*a1 > a2*)
    | EQUAL of lnode * lnode                              (*a1 == a2*)
    | NOTEQ of lnode * lnode                              (*a1 <> a2 or a1 != a2*)
    | LESSEQ of lnode * lnode                             (*a1 <= a2*)
    | LARGEREQ of lnode * lnode                           (*a1 >= a2*)
    (* lop *)
    | AT of lnode * lnode
    | DOUBLECOLON of lnode * lnode
    | STRCON of lnode * lnode
    (* else *)
    | EApp of lnode * lnode                               (* e1 e2 *)
    | EFun of node * lnode                               (* fun (x:t1) -> e *)
    | ELet of node * lnode  (* let [rec] (x1:t1) .. (xn:tn) : t = e1 in e2 *)
    | EBlock of bool * node list * lnode (* let x1 = e1 and x2 = e2 and ... xn = en in e' | let rec f1 x1 = e1 and f2 x2 = e2 ... fn xn = en in e' *)
    | EMatch of lnode * (node*lnode) list (*node = branch*)                   (* match e with bs *)
    | IF of lnode * lnode * lnode
    (*List operation*)
    | Raise of lnode 
    | Prog of node list 

    let rec type_to_node : typ -> node
    = fun typ ->
      match typ with 
      | TUnit -> TUnit
      | TInt -> TInt
      | TString -> TString
      | TBool -> TBool
      | TBase id -> TBase (Id id)
      | TList t -> TList (type_to_node t)
      | TTuple l -> if (l=[]) then TUnit else TTuple(List.map type_to_node l)
      | TArr (t1,t2) -> TArr(type_to_node t1, type_to_node t2)
      | TVar x -> TVar (Id x)
      | TCtor (x,tl) -> TCtor (type_to_node x, List.map type_to_node tl)
      | TExn -> TExn

    let user_defined_type_to_node : ctor -> node
    = fun (id,typ_lst) ->
      match typ_lst with
      | [] -> Ctor(Id (id),[])
      | hd::tl -> Ctor(Id ("Not Yet"),List.map type_to_node typ_lst)

    let rec let_to_node : let_bind -> node
    = fun x ->
      match x with
      | BindUnder -> BindUnder
      | BindOne x -> BindOne(Id(x))
      | BindTuple xs -> BindTuple(List.map let_to_node xs)

    let rec pat_to_node : pat -> node
    = fun x ->
      match x with
      | PUnit -> PUnit
      | PUnder -> PUnder 
      | PInt x -> PInt x
      | PBool b -> PBool b
      | PVar x -> PVar (Id x)
      | PList lst -> PList (List.map pat_to_node lst)
      | PCons lst -> PCons (List.map pat_to_node lst)
      | PTuple lst -> PTuple (List.map pat_to_node lst)
      | Pats lst -> Pats (List.map pat_to_node lst)
      | PCtor (x,lst) -> PCtor (Id (x), (List.map pat_to_node lst))
    
    let rec exp_to_node : lexp -> lnode
    = fun (l, exp) ->
      let node = 
      match exp with
      | EUnit -> EUnit
      | Const n -> Const(n)
      | String id -> String(Id id)
      | TRUE -> TRUE
      | FALSE -> FALSE
      | EList lst -> EList(List.map exp_to_node lst)
      | EVar x -> EVar(Id(x))
      | ECtor (x,lst) -> ECtor(Id (x), List.map exp_to_node lst)
      | ETuple lst -> ETuple(List.map exp_to_node lst)
      | ADD (e1,e2) -> ADD(exp_to_node e1, exp_to_node e2)
      | SUB (e1,e2) -> SUB(exp_to_node e1, exp_to_node e2)
      | MUL (e1,e2) -> MUL(exp_to_node e1, exp_to_node e2)
      | DIV (e1,e2) -> DIV(exp_to_node e1, exp_to_node e2)
      | MOD (e1,e2) -> MOD(exp_to_node e1, exp_to_node e2)
      | MINUS e -> MINUS (exp_to_node e)
      | OR (e1,e2) -> OR(exp_to_node e1, exp_to_node e2)
      | AND (e1,e2) -> AND(exp_to_node e1, exp_to_node e2)
      | LESS (e1,e2) -> LESS(exp_to_node e1, exp_to_node e2)
      | LARGER (e1,e2) -> LARGER(exp_to_node e1, exp_to_node e2)
      | EQUAL (e1,e2) -> EQUAL(exp_to_node e1, exp_to_node e2)
      | NOTEQ (e1,e2) -> NOTEQ(exp_to_node e1, exp_to_node e2)
      | LESSEQ (e1,e2) -> LESSEQ(exp_to_node e1, exp_to_node e2)
      | LARGEREQ (e1,e2) -> LARGEREQ(exp_to_node e1, exp_to_node e2)
      | AT (e1,e2) -> AT(exp_to_node e1, exp_to_node e2)
      | DOUBLECOLON (e1,e2) -> DOUBLECOLON(exp_to_node e1, exp_to_node e2)
      | STRCON (e1,e2) -> STRCON(exp_to_node e1, exp_to_node e2)
      | NOT e -> NOT (exp_to_node e)
      | EApp (e1,e2) -> EApp (exp_to_node e1, exp_to_node e2)
      | EFun (arg,e) -> EFun (arg_to_node arg, exp_to_node e)
      | IF (e1,e2,e3) -> IF (exp_to_node e1, exp_to_node e2, exp_to_node e3)
      | ELet (f,is_rec,args,typ,exp,exp2) -> 
        ELet (binding_to_node (f,is_rec,args,typ,exp), exp_to_node exp2)
      | EBlock (is_rec, elst, e) -> EBlock (is_rec, (List.map binding_to_node elst), exp_to_node e)
      | EMatch (e,lst) -> EMatch (exp_to_node e, List.map (fun (pat,exp) -> (pat_to_node pat,exp_to_node exp)) lst)
      | Raise e -> Raise (exp_to_node e)
      | _ -> raise Invalid_Expression_Construct
    in (l, node)

    and binding_to_node : binding -> node 
    = fun (f,is_rec,args,typ,exp) ->
      Binding (let_to_node f, is_rec, List.map arg_to_node args, 
               type_to_node typ, exp_to_node exp)
    
    and arg_to_node : arg -> node
    = fun x ->
      match x with
      | ArgUnder typ -> ArgUnder (type_to_node typ)
      | ArgOne (x,typ) -> ArgOne (Id (x),type_to_node typ) 
      | ArgTuple xs -> ArgTuple(List.map arg_to_node xs)

    let rec decl_to_node : decl -> node
    = fun decl ->
      match decl with
      (*| DExcept ctor -> DExcept (user_defined_type_to_node ctor) 
      | DEqn (x,typ) -> DEqn (Id (x), type_to_node typ) 
      | DData (id,ctor) -> DData (Id (id), List.map user_defined_type_to_node ctor) *)
      | DExcept _ -> Empty 
      | DEqn _ -> Empty 
      | DData _ -> Empty 
      | DLet bind_tuple -> DLet (binding_to_node bind_tuple)
      | DBlock (is_rec,bind_tuples) -> DBlock (is_rec, (List.map binding_to_node bind_tuples)) 
      | TBlock _ -> Empty
end


module R = struct
    type root_node = 
    | Empty | Id | TUnit | TInt | TBool | TString | TBase | TList | TTuple | TCtor | TArr | TVar | TExn
    | Ctor | PUnit | PInt | PBool | PVar | PList | PCons | PTuple | Pats | PCtor | BindUnder | BindOne | BindTuple
    | ArgUnder | ArgOne | ArgTuple | Binding | DExcept | DEqn | DData | DLet | DBlock | TBlock 
    | EUnit | Const | TRUE | FALSE | EList | String | EVar | ECtor | ETuple | ADD | SUB | MUL | DIV | MOD | MINUS
    | NOT | OR | AND | LESS | LARGER | EQUAL | NOTEQ | LESSEQ | LARGEREQ | AT | DOUBLECOLON | STRCON | EApp | EFun
    | ELet | EBlock | EMatch | IF | Raise | Prog
    [@@deriving compare]

    let init_vector =
        [(Empty ,0); (Id ,0); (TUnit ,0); (TInt ,0); (TBool ,0); (TString ,0); 
        (TBase ,0); (TList ,0); (TTuple ,0); (TCtor ,0); (TArr ,0); (TVar ,0);
        (TExn ,0); (Ctor ,0); (PUnit ,0); (PInt ,0); (PBool ,0); (PVar ,0);
        (PList ,0); (PCons ,0); (PTuple ,0); (Pats ,0); (PCtor ,0); (BindUnder ,0);
        (BindOne ,0); (BindTuple ,0); (ArgUnder ,0); (ArgOne ,0); (ArgTuple ,0);
        (Binding ,0); (DExcept ,0); (DEqn ,0); (DData ,0); (DLet ,0); (DBlock ,0);
        (TBlock ,0); (EUnit ,0); (Const ,0); (TRUE ,0); (FALSE ,0); (EList ,0);
        (String ,0); (EVar ,0); (ECtor ,0); (ETuple ,0); (ADD ,0); (SUB ,0);
        (MUL ,0); (DIV ,0); (MOD ,0); (MINUS ,0); (NOT ,0); (OR ,0); (AND ,0);
        (LESS ,0); (LARGER ,0); (EQUAL ,0); (NOTEQ ,0); (LESSEQ ,0); (LARGEREQ ,0);
        (AT ,0); (DOUBLECOLON ,0); (STRCON ,0); (EApp ,0); (EFun ,0); (ELet ,0); (EBlock ,0);
        (EMatch ,0); (IF ,0); (Raise ,0); (Prog, 0);]
(*
    let lookup_root : N.lnode -> 
    
    let unlabel_node : N.lnode -> N.node
    = fun (l,n) -> 
      match n with
      | Binding (f,b,lst,n,ln) -> unlabel_node ln
      | EList lst -> EList List.map 
      | _ -> na
*)


end

    module AstMap = BatMap.Make(struct type t = R.root_node let compare = compare end)
    
    (*vector*)
    type t = (R.root_node*int) list
    
    let ast_flatten : prog -> N.node list
    = fun prog -> 
      let flat = List.map N.decl_to_node prog in
      List.filter (fun x -> x <> N.Empty) flat 

    let rec vectorize: prog -> t
    = fun prog -> 
      let ast = ast_flatten prog in
      [(Id,0);
       (Id,0);
       (Id,0);]

    let print_list : t -> unit
    = fun lst ->
      let cnt = List.map (fun (_,x) -> x) lst in
      let rec traverse = fun x -> 
        match x with
        | [] -> print_endline "end of list"
        | hd::tl -> print_endline (string_of_int hd); traverse tl in
      traverse cnt
      



