(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = match lst with
|[] -> []
|[x] -> if (pred x = true) then [x] else []
|h::t -> (filter pred [h]) @ (filter pred t)
