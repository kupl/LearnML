let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> 
  match n with
    | 0 -> fun x -> x 
    | _-> fun x -> f (iter(n-1,f) x);;
  
  
  
iter (2, fun x -> x + 2) 0;;

