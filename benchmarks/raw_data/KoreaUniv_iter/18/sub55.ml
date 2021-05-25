let mix f g
= fun x->
  g (f x) ;; 
  
let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
  if n = 0 then fun x -> x
  else fun x -> mix (iter(n-1, f)) f x;;
  
iter (2, fun x -> 2+x) 0;;