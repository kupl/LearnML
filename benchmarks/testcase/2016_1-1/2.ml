let rec f : int -> int
= fun n -> if n = 0 then 1 else if n = 1 then 1 else f (n-1) + f (n-2);;

