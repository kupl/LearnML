
(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst =
 if pred(hd) then hd::pred(tl) else pred(tl);;

