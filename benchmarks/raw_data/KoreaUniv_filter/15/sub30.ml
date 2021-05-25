(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst =
  match lst with
  | [] -> []
  | hd :: tl ->
      let new_tl = filter pred tl in
      if pred hd then hd :: new_tl else new_tl
;;
