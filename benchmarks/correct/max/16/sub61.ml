(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> List.fold_left (fun x y -> if y > x then y else x) (List.hd lst) lst
 