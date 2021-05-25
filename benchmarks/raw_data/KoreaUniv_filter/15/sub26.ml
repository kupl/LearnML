(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst
= match lst with
    | [] -> []
    | head::tail -> if pred head
        then head::(filter pred tail)
        else filter pred tail;;
