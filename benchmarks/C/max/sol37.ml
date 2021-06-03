(* Problem 3 *)
let rec max l =
match l with
[] -> raise (Failure "Empty List")
|[hd] -> hd
|hd::tl -> if (hd > (max tl)) then hd else (max tl);;
 