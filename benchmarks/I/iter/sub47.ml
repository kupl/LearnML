let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> let compose f g = fun x -> f(g(x)) in if n==0 || n==1 then f else compose f (iter(n-1,f));;

iter(6, fun x -> 2+x) 0;;