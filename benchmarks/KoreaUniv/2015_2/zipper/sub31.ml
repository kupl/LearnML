let rec zipper : int list * int list -> int list 
=fun (a,b) ->  (* TODO *) 
 
match a, b with 
    |[],_ -> b 
    |_,[] -> a 
    |hd1::tl1, hd2::tl2 -> [hd1] @ [hd2] @ (zipper (tl1, tl2));; 
 
 