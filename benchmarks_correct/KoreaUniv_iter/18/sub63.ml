let iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> 
  if n = 0 then fun a -> a
  else 
    let rec foo n f 
    = if n = 1 then fun a -> f a
      else 
        fun a -> foo (n - 1) f (f a)
    in foo n f;;
