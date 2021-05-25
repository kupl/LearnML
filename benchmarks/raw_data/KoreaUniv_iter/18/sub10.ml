let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> 
  if n=0 then fun n->n
  else fun x -> f (iter(n-1,f) x);;
iter (0,fun x-> 2+x) 0;;
iter (20,fun x-> 2+x) 0;;
iter (123,fun x-> 2+x) 0;;