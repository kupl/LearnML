let rec f : int -> int
= fun n -> if(n=1||n=2) then n-1 else f(n-1)+f(n-2);; 

