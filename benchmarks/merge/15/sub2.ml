let rec merge (a, b) =
  let n = fst (a, b) in
  let m = snd (a, b) in
  match n, m with
   | [], _ -> m
   | _, [] -> n
   | hn :: tn, hm :: tm -> 
     if hn < hm 
      then hm :: merge (tm, (hn :: tn))  
      else hn :: merge (tn, (hm :: tm))




