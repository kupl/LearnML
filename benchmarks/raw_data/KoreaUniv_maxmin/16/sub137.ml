(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> 
  List.fold_left (fun a b -> if (a > b) then a else b) (List.hd lst) lst;;
 