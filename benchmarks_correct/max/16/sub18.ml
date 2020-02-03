(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> if List.length lst = 1 then List.hd lst else
  match lst with 
  | [] -> 0 
  | hd::tl -> if hd > max tl then hd else max tl
  ;;
 