(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> match lst with
	|[] -> 0
	|hd::tl -> List.fold_left (fun x y -> if x > y then x else y) hd lst;; 
 