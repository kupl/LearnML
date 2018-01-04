let rec iter (n: int) (f: 'a -> 'a) =
  let f_n x = f(iter (n-1) f x) in
  if n <= 0 then (fun x -> x)
  else f_n
