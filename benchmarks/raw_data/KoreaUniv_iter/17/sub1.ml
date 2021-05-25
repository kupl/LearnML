let iter : int * (int->int) -> (int->int)
  = fun (n,f) -> let rec iters(n,f) x = if n = 0 then (fun n -> n) x else if n = 1 then f x else f (iters(n-1,f) x) in iters(n,f)
