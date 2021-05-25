(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter func lst =
  match lst with
  |[] -> []
  |hd::tl -> if func hd = true then hd::(filter func tl) else filter func tl
