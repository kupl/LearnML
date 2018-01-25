
  let rec product f a b =
    if a=b then f a
      else (f b)*product f a (b-1);;
      