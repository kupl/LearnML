(*problem 3*)
  let comp f g = fun x -> g (f x);;
  let rec iter =
    fun (n,f) -> if n=0 then fun x->x
                 else 
                  if n=1 then f
                  else comp f (iter(n-1,f));;