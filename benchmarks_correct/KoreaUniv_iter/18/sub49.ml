let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> 
  if n <= 0 then fun x -> x 
  else fun x -> iter(n - 1, f) (f x);;

iter(5, fun x -> 2+x) 0;;