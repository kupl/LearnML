let rec f : int list -> int
= fun lst ->  
    match lst with
    |[a] -> a 
    |hd::tl -> if hd < (f tl) then hd else (f tl);; 
