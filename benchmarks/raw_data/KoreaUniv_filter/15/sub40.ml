(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst
= match lst with
  | [] -> []
  | hd::rest -> if (pred hd) then hd::(filter pred rest) 
    else filter pred rest
