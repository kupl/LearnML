let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> 
  let rec compose f g = fun x ->f(g(x)) in
    if n<2 then f else
      compose f(iter(n-1,f));;
      
iter(2,fun x-> 2+x) 0;;
