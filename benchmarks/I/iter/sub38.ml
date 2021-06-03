let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->let compose f g = fun x -> f(g(x)) in
 if n<=1 then f else compose f (iter(n-1,f));;
 
iter(0, fun x -> 2+x) 0;;
iter(1, fun x -> 2+x) 0;;
iter(5, fun x -> 2+x) 0;;
(*TODO*)
