exception Error of string

let rec iter ((n : int), (f : 'a -> 'a)) : 'a -> 'a =
  let identity x = x in

  let rec ff x = f (f x) in
  if n < 0 then raise Error "Garbage In"
  else match n with 0 -> identity | 1 -> f | _ -> iter (n - 1, ff)
