let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> 
  if n == 0 then fun x -> x
  else fun v ->  iter (n - 1, f) (f v);;
  
iter(10, fun x -> 2+x) 0;;
