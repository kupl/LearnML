(* problem 6*) 
  
let rec drop : 'a list -> int -> 'a list 
= fun l n -> 
match l with
|[] -> []
|hd::tl -> if n>1 then drop tl (n-1) else tl