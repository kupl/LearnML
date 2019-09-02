open Lang
open BatVect
open Node

exception Table_Update_Failure

module Fv = Freq_vector
(*node with height*)
type t = (int BatVect.t) list

let init_vector = List.map (fun (s,num) -> 
    (s, BatVect.empty)) Fv.init_vector 

(*Question: is Hashtable create size dependent to vector size*)
let init_tbl : unit -> (string, int BatVect.t) BatHashtbl.t
= fun x ->
  let tbl = BatHashtbl.create 100 in
  let rec iter : (string, int BatVect.t) BatHashtbl.t -> (string * int BatVect.t) list -> (string, int BatVect.t) BatHashtbl.t
  = fun tbl lst ->
    match lst with
    | [] -> tbl
    | (hd,vec)::tl -> BatHashtbl.replace tbl hd vec; iter tbl tl
  in iter tbl init_vector 

let rec node_height_cal : node -> node
= fun node ->
  let rec height : node -> int 
  = fun n ->
    match n with 
    | Leaf -> 0
    | Node(h,s,lst) -> 1 + (BatList.max (List.map height lst))
    | LNode(l,n) -> height n 
    | Empty -> raise (Failure "posvec node_height_cal: node Empty") in
  match node with
  | Node(h,s,lst) as n-> Node (height n,s,lst)
  | LNode(l,n) -> node_height_cal n
  | _ -> node 

let update : (string, int BatVect.t) BatHashtbl.t -> string -> int -> unit
= fun tbl s h -> 
  let cur_v = 
    try BatHashtbl.find tbl s
    with Not_found -> raise (Failure "pos vec update: Not found key")
  BatHashtbl.replace tbl s (BatVect.append h cur_v)

let rec traverse : (string, int BatVect.t) BatHashtbl.t -> node -> unit
= fun tbl node ->
  match node with
  | LNode (l,n) -> traver tbl n
  | Node (h,s,lst) -> update tbl s h;
    List.iter (traverse tbl) lst
  | Leaf -> ()
  | Empty -> raise (Failure "pos_vec traverse: Empty node")
  
let node_vectorize = node -> t 
= fun node -> 
  let tbl = init_tbl () in
  let hnode = node_height_cal in
  let table = traverse tbl hnode in
  BatHashtbl.to_list table |> List.sort compare |> List.map (fun (k,v) -> v)

let rec funcs_vectorize : (string * lexp) list -> (string * t) list
= fun lst ->
  match lst with
  | [] -> []
  | (f,lexp)::tl ->
    let vec = lexp |> exp_to_node |> node_vectorize in
    (f, vec) :: (funcs_vectorize tl)

let calculate_distance : t -> t -> float
= fun t1 t2 -> 0.0 (*Not Implemented*)

(*
let

let convert_to_height : int -> pnode -> pnode

let update : (string, int BatVect.t) BatHashtbl.t -> string -> int -> unit
= fun tbl s depth ->
  let cur = 
    try BatHashtbl.find tbl s
    with Not_found -> raise Table_Update_Failure in
  BatHashtbl.replace tbl s 

let rec traverse : (string, int BatVect.t) BatHashtbl.t -> node -> int -> unit
= fun tbl node depth ->
  match node with 
  | LNode (l,n) -> traverse tbl n
  | Node (s,lst) -> update tbl s;
    List.iter (traverse tbl) lst
  | Id id -> update tbl 
*)
