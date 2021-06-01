let rec iter : int * ('a -> 'a) -> 'a -> 'a 
= fun (n,g) -> 
  fun k ->
  if n > 0 then g (iter ( n-1 , g) k)
  else k
