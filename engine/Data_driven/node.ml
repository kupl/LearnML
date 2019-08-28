open Lang
open Util
open Type

exception Invalid_Expression_Construct

(* Normalize lang types -> node type
 *
 *)

type node = 
  |LNode of label * node 
  |Node of string * (node list)          
  |Id of string
  |Const of int
  |String of string 
  |Bool of bool 
  |Empty 

let rec_to_node : bool -> node
= fun b ->
  match b with
  | true -> Node ("Rec", [])
  | false -> Node ("No Rec", [])

let rec pat_to_node : pat -> node
= fun x ->
  match x with
  | PUnit -> Node ("PUnit",[])
  | PUnder -> Node ("PUnder",[])
  | PInt x -> Node ("PInt",[Const x])
  | PBool b -> Node ("PBool",[Bool b])
  | PVar x -> Node ("PVar", [Id x])
  | PList lst -> Node ("PList", List.map pat_to_node lst)
  | PCons lst -> Node ("PCons", List.map pat_to_node lst) 
  | PTuple lst -> Node ("PTuple", List.map pat_to_node lst) 
  | Pats lst -> Node ("Pats", List.map pat_to_node lst) 
  | PCtor (x,lst) -> Node ("PCtor", (Id x)::(List.map pat_to_node lst))
    
let rec type_to_node : typ -> node
= fun t ->
  match t with
  | TUnit -> Node ("TUnit",[])
  | TInt -> Node ("TInt",[])
  | TString -> Node ("TString",[])
  | TBool -> Node ("TBool",[])
  | TBase id -> Node ("TBase",[Id id])
  | TList t -> Node ("TList", [type_to_node t])
  | TTuple l -> if (l=[]) then type_to_node TUnit
                          else Node ("TTuple", List.map type_to_node l)
  | TArr (t1,t2) -> Node ("TArr", [type_to_node t1; type_to_node t2])
  | TVar x -> Node ("TVar",[Id x])
  | TCtor (x,tl) -> Node ("TCtor", (type_to_node x)::(List.map type_to_node tl))
  | TExn -> Node ("TExn",[])

let rec let_to_node : let_bind -> node
= fun bind ->
  match bind with 
  | BindUnder -> Node ("BindUnder", [])
  | BindOne x -> Node ("BindOne", [Id x])
  | BindTuple xs -> Node ("BindTuple", List.map let_to_node xs)

let rec arg_to_node : arg -> node
= fun arg ->
  match arg with
  | ArgUnder typ -> Node ("ArgUnder", [type_to_node typ])
  | ArgOne (x,typ) -> Node ("ArgOne", [Id x;type_to_node typ])
  | ArgTuple xs -> Node ("ArgTuple", List.map arg_to_node xs)

let rec exp_to_node : lexp -> node 
= fun (l,exp) ->
  let f : exp -> node
  = fun exp ->
  match exp with
  | EUnit -> Node ("EUnit", [])
  | Const n -> Const n
  | String id-> String id
  | TRUE -> Bool true
  | FALSE -> Bool false
  | EList lst -> Node ("EList", List.map exp_to_node lst)
  | EVar x -> Node ("EVar", [Id x]) 
  | ECtor (x,lst) -> Node ("ECtor", Id (x)::(List.map exp_to_node lst))
  | ETuple lst -> Node ("ETuple", List.map exp_to_node lst)
  | ADD (e1,e2) -> Node ("ADD", [exp_to_node e1;exp_to_node e2])
  | SUB (e1,e2) -> Node ("SUB", [exp_to_node e1;exp_to_node e2])
  | MUL (e1,e2) -> Node ("MUL", [exp_to_node e1;exp_to_node e2])
  | DIV (e1,e2) -> Node ("DIV", [exp_to_node e1;exp_to_node e2])
  | MOD (e1,e2) -> Node ("MOD", [exp_to_node e1;exp_to_node e2])
  | MINUS e -> Node ("MINUS", [exp_to_node e])
  | OR (e1,e2) -> Node ("OR", [exp_to_node e1;exp_to_node e2])
  | AND (e1,e2) -> Node ("AND", [exp_to_node e1;exp_to_node e2])
  | LESS (e1,e2) -> Node ("LESS", [exp_to_node e1;exp_to_node e2])
  | LARGER (e1,e2) -> Node ("LARGER", [exp_to_node e1;exp_to_node e2])
  | EQUAL  (e1,e2) -> Node ("EQUAL", [exp_to_node e1;exp_to_node e2])
  | NOTEQ (e1,e2) -> Node ("NOTEQ", [exp_to_node e1;exp_to_node e2])
  | LESSEQ (e1,e2) -> Node ("LESSEQ", [exp_to_node e1;exp_to_node e2])
  | LARGEREQ (e1,e2) -> Node ("LARGEREQ", [exp_to_node e1;exp_to_node e2])
  | AT (e1,e2) -> Node ("AT", [exp_to_node e1;exp_to_node e2])
  | DOUBLECOLON (e1,e2) -> Node ("DOUBLECOLON", [exp_to_node e1;exp_to_node e2])
  | STRCON (e1,e2) -> Node ("STRCON", [exp_to_node e1;exp_to_node e2])
  | NOT e -> Node ("NOT", [exp_to_node e])
  | EApp (e1,e2) -> Node ("EApp", [exp_to_node e1;exp_to_node e2])
  | EFun (arg,e) -> Node ("EFun", [arg_to_node arg;exp_to_node e])
  | IF (e1,e2,e3) -> Node ("IF", [exp_to_node e1;exp_to_node e2;exp_to_node e3;])
  | ELet (f,is_rec,args,typ,exp,exp2) ->
    Node ("ELet", [binding_to_node (f,is_rec,args,typ,exp); exp_to_node exp2])
  | EBlock (is_rec, elst, e) -> 
    Node ("EBlock", (rec_to_node is_rec)::
                    (List.map binding_to_node elst)@[exp_to_node e])
  | EMatch (e,lst) -> 
    let rec aux = fun lst -> begin
    match lst with 
    | [] -> []
    | (p,lexp)::tl -> (pat_to_node p)::(exp_to_node lexp)::(aux tl) end 
    in Node ("EMatch", (exp_to_node e)::(aux lst))
  | Raise e -> Node ("Raise", [])
  | _ -> raise Invalid_Expression_Construct in 
    let node = f exp in 
  LNode (l, node)

and binding_to_node : binding -> node
= fun (f,is_rec,args,typ,exp) ->
  Node ("Binding", [let_to_node f; rec_to_node is_rec]@(List.map arg_to_node args)@[type_to_node typ; exp_to_node exp])

let decl_to_node : decl -> node
= fun decl -> 
  match decl with
  | DExcept _ -> Empty
  | DEqn _ -> Empty
  | DData _ -> Empty
  | DLet bind_tuple -> Node ("DLet", [(binding_to_node bind_tuple)])
  | DBlock (is_rec,bind_tuples) -> Node ("DBlock", rec_to_node is_rec :: (List.map binding_to_node bind_tuples))
  | TBlock _ -> Empty

