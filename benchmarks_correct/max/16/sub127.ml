(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> (* TODO *)
match lst with
[] -> raise (Failure "Must Not Be Empty!")
|[one] -> one
|hd::tl -> if ( hd > (max tl) ) then hd else (max tl)
 