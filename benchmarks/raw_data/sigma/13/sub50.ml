let sigma (a, b, f) =
  let rec aux (cur, acc) =
    if cur > b then acc
  	else aux (cur+1, acc+f(cur))
  in aux(a, 0)