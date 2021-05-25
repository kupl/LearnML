
(* Problem 1: filter *)
let rec filter pred lst =
  match lst with
  | [] -> []
  | hd::tl -> if pred hd = false then (filter pred tl)
  else hd::(filter pred tl)

