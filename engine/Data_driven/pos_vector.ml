open Lang
open BatVect
open Node

module Fv = Freq_vector
type t = (int BatVect.t) BatVect.t

let freq_init_vector = Fv.init_vector
let vec_size = List.length freq_init_vector

let init : unit -> t
= fun () ->
  let each_vec = BatVect.make 1 0 in 
  let test' = BatVect.make vec_size each_vec in
  test'
 


