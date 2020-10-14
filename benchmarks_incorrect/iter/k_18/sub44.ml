let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> (*TODO*)
match n with 
  0 ->  f
  |_ -> iter ((n-1), fun f -> f );;

    
    
iter(1, fun x ->2+x);;