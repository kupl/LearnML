let rec merge (xs,ys) = match xs with
  | [] -> ys
  | hxs::txs -> 
      if hxs > (match ys with
              | [] -> hxs
              | hys::tys -> hys)
      then hxs :: merge (txs,ys)
      else match ys with
           | [] -> xs
           | hys::tys -> hys :: merge (xs,tys)

