let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> fun b -> if n = 0 then b else iter (n-1,f) (f b);; 

