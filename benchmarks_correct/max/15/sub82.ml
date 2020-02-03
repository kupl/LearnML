let rec max : int list -> int
=fun l -> match l with  
  |[]->raise(Failure "list is too short")
  |[x] -> x
  |hd::tl -> (if hd > (max tl) then hd    
  else (max tl)) 
 