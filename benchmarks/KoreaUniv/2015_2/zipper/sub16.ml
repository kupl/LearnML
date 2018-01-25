let rec zipper : int list * int list -> int list
=fun (a,b) -> 
let result = [] in 
match a with 
[] -> if b = [] then result else result@b
| hd::tl -> (match b with
[] -> if tl =[] then result@[hd] else result@[hd]@(zipper (tl, []))  
| hd_b::tl_b -> hd :: hd_b :: (zipper (tl, tl_b)) @ result)
