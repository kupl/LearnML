let iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> (*TODO*)
  let f_original = f in
  
  let rec iter2 (n,f) = 
    if n=0 then fun x->x
    else if n = 1 then f
    else iter2 (n-1, fun x -> f_original(f x))
  in iter2(n,f);;
  
iter(10,fun x-> x*2) 5;;