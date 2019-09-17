open Lang
open Util
open Type

exception Invalid_Expression_Construct

(* Normalize lang types -> node type
 * empty list means leaf node
 * LNode : labeled exp node
 * Node : exp, pattern, arguments node 
 * Empty : unnecessary declarations node 
 *)

type node = 
  |LNode of label * node 
  |Node of height * string * (node list) 
  |Leaf 
  |Empty
and height = int 

let rec node_to_string : node -> string
= fun n -> 
  match n with
  |LNode(l,n) -> node_to_string n
  |Node(h,s,lst) -> " " ^ s ^ "\n" ^ if (lst = []) then "!empty node!" ^ s else List.fold_left (fun acc elem -> acc ^ (node_to_string elem)) "" lst
  |Leaf -> "Leaf\n"
  |Empty -> ""

let uninit = max_int

let rec_to_node : bool -> node
= fun b ->
  match b with
  | true -> Node (uninit, "Rec", [Leaf])
  | false -> Node (uninit, "No Rec", [Leaf])

let rec pat_to_node : pat -> node
= fun x ->
  match x with
  | PUnit -> Node (uninit, "PUnit",[Leaf])
  | PUnder -> Node (uninit, "PUnder",[Leaf])
  | PInt x -> Node (uninit, "PInt",[Leaf])
  | PBool b -> Node (uninit, "PBool",[Leaf])
  | PVar x -> Node (uninit, "PVar", [Leaf])
  | PList lst -> let subnode = if (lst = []) then [Leaf]
                 else List.map pat_to_node lst in 
                 Node (uninit, "PList", subnode)
  | PCons lst -> let subnode = if (lst = []) then [Leaf]
                 else List.map pat_to_node lst in 
                 Node (uninit, "PCons", subnode)
  | PTuple lst -> let subnode = if (lst = []) then [Leaf]
                  else List.map pat_to_node lst in 
                  Node (uninit, "PTuple", subnode) 
  | Pats lst -> let subnode = if (lst = []) then [Leaf]
                else List.map pat_to_node lst
                in Node (uninit, "Pats", subnode) 
  | PCtor (x,lst) -> let subnode = if (lst = []) then [Leaf]
                 else List.map pat_to_node lst
                 in Node (uninit, "PCtor", subnode) 
    
let rec type_to_node : typ -> node
= fun t ->
  match t with
  | TUnit -> Node (uninit, "TUnit",[Leaf])
  | TInt -> Node (uninit, "TInt",[Leaf])
  | TString -> Node (uninit, "TString",[Leaf])
  | TBool -> Node (uninit, "TBool",[Leaf])
  | TBase id -> Node (uninit, "TBase",[Leaf])
  | TList t -> Node (uninit, "TList", [type_to_node t])
  | TTuple l -> if (l=[]) then type_to_node TUnit
                          else Node (uninit, "TTuple", List.map type_to_node l)
  | TArr (t1,t2) -> Node (uninit, "TArr", [type_to_node t1; type_to_node t2])
  | TVar x -> Node (uninit, "TVar",[Leaf])
  | TCtor (x,tl) -> Node (uninit, "TCtor", (type_to_node x)::(List.map type_to_node tl))
  | TExn -> Node (uninit, "TExn",[Leaf])

let rec let_to_node : let_bind -> node
= fun bind ->
  match bind with 
  | BindUnder -> Node (uninit, "BindUnder", [Leaf])
  | BindOne x -> Node (uninit, "BindOne", [Leaf])
  | BindTuple xs -> Node (uninit, "BindTuple", List.map let_to_node xs)

let rec arg_to_node : arg -> node
= fun arg ->
  match arg with
  | ArgUnder typ -> Node (uninit, "ArgUnder", [type_to_node typ])
  | ArgOne (x,typ) -> Node (uninit, "ArgOne", [type_to_node typ])
  | ArgTuple xs -> Node (uninit, "ArgTuple", List.map arg_to_node xs)

let rec exp_to_node : lexp -> node 
= fun (l,exp) ->
  let f : exp -> node
  = fun exp ->
  match exp with
  | EUnit -> Node (uninit, "EUnit", [Leaf])
  | Const n -> Node (uninit, "Const_Int", [Leaf])
  | String id-> Node (uninit, "Const_String", [Leaf])
  | TRUE 
  | FALSE -> Node(uninit, "Const_Bool",[Leaf])
  | EList lst -> let subnode = if (lst = []) then [Leaf]
                 else List.map exp_to_node lst in 
                 Node (uninit, "EList", subnode)
  | EVar x -> Node (uninit, "EVar", [Leaf]) 
  | ECtor (x,lst) -> let subnode = if (lst = []) then [Leaf]
                     else List.map exp_to_node lst in
                     Node (uninit, "ECtor", subnode)
  | ETuple lst -> Node (uninit, "ETuple", List.map exp_to_node lst)
  | ADD (e1,e2) -> Node (uninit, "ADD", [exp_to_node e1;exp_to_node e2])
  | SUB (e1,e2) -> Node (uninit, "SUB", [exp_to_node e1;exp_to_node e2])
  | MUL (e1,e2) -> Node (uninit, "MUL", [exp_to_node e1;exp_to_node e2])
  | DIV (e1,e2) -> Node (uninit, "DIV", [exp_to_node e1;exp_to_node e2])
  | MOD (e1,e2) -> Node (uninit, "MOD", [exp_to_node e1;exp_to_node e2])
  | MINUS e -> Node (uninit, "MINUS", [exp_to_node e])
  | OR (e1,e2) -> Node (uninit, "OR", [exp_to_node e1;exp_to_node e2])
  | AND (e1,e2) -> Node (uninit, "AND", [exp_to_node e1;exp_to_node e2])
  | LESS (e1,e2) -> Node (uninit, "LESS", [exp_to_node e1;exp_to_node e2])
  | LARGER (e1,e2) -> Node (uninit, "LARGER", [exp_to_node e1;exp_to_node e2])
  | EQUAL  (e1,e2) -> Node (uninit, "EQUAL", [exp_to_node e1;exp_to_node e2])
  | NOTEQ (e1,e2) -> Node (uninit, "NOTEQ", [exp_to_node e1;exp_to_node e2])
  | LESSEQ (e1,e2) -> Node (uninit, "LESSEQ", [exp_to_node e1;exp_to_node e2])
  | LARGEREQ (e1,e2) -> Node (uninit, "LARGEREQ", [exp_to_node e1;exp_to_node e2])
  | AT (e1,e2) -> Node (uninit, "AT", [exp_to_node e1;exp_to_node e2])
  | DOUBLECOLON (e1,e2) -> Node (uninit, "DOUBLECOLON", [exp_to_node e1;exp_to_node e2])
  | STRCON (e1,e2) -> Node (uninit, "STRCON", [exp_to_node e1;exp_to_node e2])
  | NOT e -> Node (uninit, "NOT", [exp_to_node e])
  | EApp (e1,e2) -> Node (uninit, "EApp", [exp_to_node e1;exp_to_node e2])
  | EFun (arg,e) -> Node (uninit, "EFun", [arg_to_node arg;exp_to_node e])
  | IF (e1,e2,e3) -> Node (uninit, "IF", [exp_to_node e1;exp_to_node e2;exp_to_node e3;])
  | ELet (f,is_rec,args,typ,exp,exp2) ->
    Node (uninit, "ELet", [binding_to_node (f,is_rec,args,typ,exp); exp_to_node exp2])
  | EBlock (is_rec, elst, e) -> 
    Node (uninit, "EBlock", (rec_to_node is_rec)::
                    (List.map binding_to_node elst)@[exp_to_node e])
  | EMatch (e,lst) -> 
    let rec aux = fun lst -> begin
    match lst with 
    | [] -> []
    | (p,lexp)::tl -> (pat_to_node p)::(exp_to_node lexp)::(aux tl) end 
    in Node (uninit, "EMatch", (exp_to_node e)::(aux lst))
  | Raise e -> Node (uninit, "Raise", [Leaf])
  | _ -> raise Invalid_Expression_Construct in 
    let node = f exp in 
  LNode (l, node)

and binding_to_node : binding -> node
= fun (f,is_rec,args,typ,exp) ->
  Node (uninit, "Binding", [let_to_node f; rec_to_node is_rec]@(List.map arg_to_node args)@[type_to_node typ; exp_to_node exp])

let decl_to_node : decl -> node
= fun decl -> 
  match decl with
  | DExcept _ -> Empty
  | DEqn _ -> Empty
  | DData _ -> Empty
  | DLet bind_tuple -> Node (uninit, "DLet", [(binding_to_node bind_tuple)])
  | DBlock (is_rec,bind_tuples) -> Node (uninit, "DBlock", rec_to_node is_rec :: (List.map binding_to_node bind_tuples))
  | TBlock _ -> Empty

