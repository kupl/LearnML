let rec iter (n:int) (f: int->int) =
  let identity x = x in
  let ans x = (iter (n-1) f) (f x) in
  
  match n with
  | 0 -> identity
  | _ -> ans
  
