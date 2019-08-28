open Lang
open BatVect
open Node

exception Table_Update_Failure

module Fv = Freq_vector
(*node with height*)
type pnode = node * int 
type t = (int BatVect.t) BatVect.t

let freq_init_vector = Fv.init_vector
let vec_size = List.length freq_init_vector

let init_tbl : unit -> t
= fun () ->
  let each_vec = BatVect.make 1 0 in 
  let test' = BatVect.make vec_size each_vec in
  test'
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
