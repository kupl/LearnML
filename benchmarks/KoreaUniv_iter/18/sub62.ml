let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> let identity x = x in 
let compose h g = fun x -> h(g(x)) in
if n=0 then  identity else compose f iter(n-1,f);;

iter(5, fun x-> 2+x) 0;;

