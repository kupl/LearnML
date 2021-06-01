(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> 
match lst with
| [] -> 0
| hd::tl -> ( if tl = [] then hd else ( if hd < (max tl) then ( max tl ) else hd ) )
 