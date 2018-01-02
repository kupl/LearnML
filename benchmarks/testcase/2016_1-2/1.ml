let rec f : int -> int -> int
= fun n1 n2 -> if n2 = 0 then 1 else if n1 = n2 then 1 
else (f (n1-1) (n2-1)) + (f (n1-1) (n2-1));; 
