let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if a>b then 0 
               else if f=f then sigma(f,a+1,b) 
               else sigma(f a,a+1,b);;
