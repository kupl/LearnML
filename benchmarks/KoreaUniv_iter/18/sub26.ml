let iter : int * (int -> int) -> (int -> int)

= fun (n,f) ->  if n>1 then fun(n-1, f(f)) else f

iter(n, fun x -> 2+x);;