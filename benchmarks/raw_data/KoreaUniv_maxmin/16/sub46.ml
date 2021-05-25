
(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> match lst with
| [] -> raise (Failure "The list is empty.")
| hd::[] -> hd
| hd::tl -> let m = max(tl) in
if (hd < m) then m
else hd
 