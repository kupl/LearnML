let prime : int -> bool
= fun n -> let rec range n = 
             if n = 1 then []
             else n::(range (n-1))
           in
             let rec fold_and l =
               match l with
               | [] -> true
               | hd::tl -> hd && (fold_and tl)
             in
               let rec map_mod n l =
                 match l with
                 | [] -> []
                 | hd::tl -> (n mod hd != 0)::(map_mod n tl)
               in
                 if n = 1 then false
                 else fold_and (map_mod n (range (n-1)));;
