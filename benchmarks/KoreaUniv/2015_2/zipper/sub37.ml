let rec zipper : int list * int list -> int list
=fun (a,b) -> 
        match a,b with
        |[], [] -> []
        |a, [] -> a
        |[],b -> b 
        |hd1::tl1 , hd2::tl2 -> hd1::hd2::(zipper (tl1,tl2)) 
