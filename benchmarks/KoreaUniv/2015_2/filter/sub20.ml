(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst =
  match lst with
  |[q] -> if pred q then [q] else []
  |h::t -> if pred h then h::(filter pred t) else filter pred t
