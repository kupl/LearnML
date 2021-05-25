(*********************)(*2014210077김준영*)
(* Problem 1: filter *)
(*********************)

let rec filter p l =
   match l with 
   | []-> []
   | hd::tl ->if p hd then hd::(filter p tl) 
    else (filter p tl)
