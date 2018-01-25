let rec iter (n,f) = if n=0 then (fun x->x+0) else (fun x->f(iter(n-1,f) x));;
  
