let rec max : int list -> int
=fun l -> 
 match l with 
   [] -> raise (Failure "Empty List")
  |hd::[] -> hd
  |hd::tl -> let m = (max tl) in if hd > m then hd else m