let sigma (x,y,f) : int =
  let rec sigma' acc a b f =
    if a>b then acc
    else (f a) + sigma' acc (a+1) b f
  in
  sigma' 0 x y f
    
