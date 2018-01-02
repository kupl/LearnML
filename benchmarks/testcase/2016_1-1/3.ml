let rec f : int -> int
= fun n -> if n=1 then 0 else if n=2 then 1 else  f(n-1)+f(n-2);;

