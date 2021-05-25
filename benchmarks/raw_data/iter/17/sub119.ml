let rec iter (a,f) =
    if a=0 || a<0 then function x-> x
      else function x -> iter(a-1, f) (f x)

