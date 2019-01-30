let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) k-> match (n,f) with
  (0,_) -> iter(1, fun x->x)k
 |(1,_) -> f k
 |(_,_) -> iter((n-1), f)(f k);;

iter (12, fun x-> x+2) 4;;

