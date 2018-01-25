let rec iter : int * (int -> int) -> (int -> int)
= fun (n, f) x -> if n == 0 then x 
        else (if x > 0 then iter (n - 1, f) (f x ) 
        else failwith "Error");;
